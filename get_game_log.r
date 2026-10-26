# functions needed in this file:

save_with_stamp <- function(obj, path) {
  attr(obj, "fetched_at") <- Sys.time()
  saveRDS(obj, path)
  obj
}

cache_stamp <- function(path) {
  if (!file.exists(path)) return(NULL)
  attr(readRDS(path), "fetched_at")
}

cache_age_hours <- function(path) {
  stamp <- cache_stamp(path)
  if (is.null(stamp)) return(Inf)   # old caches without a stamp count as stale
  as.numeric(difftime(Sys.time(), stamp, units = "hours"))
}

fetch_fg_json <- function(url, tries = 3) {
  status <- NA
  for (i in seq_len(tries)) {
    res <- httr::GET(url,
                     httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"),
                     httr::timeout(60))
    status <- httr::status_code(res)
    txt <- httr::content(res, "text", encoding = "UTF-8")
    if (status == 200 && !startsWith(trimws(txt), "<")) {
      return(jsonlite::fromJSON(txt))
    }
    message("FanGraphs returned non-JSON (status ", status, "), attempt ", i, "/", tries)
    Sys.sleep(2^i)
  }
  stop("FanGraphs returned non-JSON after ", tries, " tries (last status ", status, ")")
}

get_current_season <- function() {
  today <- Sys.Date()
  year <- as.integer(format(today, "%Y"))
  md <- format(today, "%m-%d")
  
  if (md < "03-15") {
    return(year - 1)
  } else {
    return(year)
  }
}

fg_minors_season <- function(stat = c("bat", "pit"), season) {
  stat <- match.arg(stat)
  dir.create("cache/fg_leaderboards", recursive = TRUE, showWarnings = FALSE)
  path <- file.path("cache/fg_leaderboards", paste0(stat, "_", season, ".rds"))
  
  if (file.exists(path)) {
    # finished seasons never change; the current season refreshes daily
    if (season < get_current_season() || cache_age_hours(path) < 24) {
      return(readRDS(path))
    }
  }
  
  url <- paste0("https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=",
                stat, "&qual=0&type=0&team=0,to&season=", season,
                "&seasonEnd=", season, "&org=&ind=0&splitTeam=true")
  
  raw <- fetch_fg_json(url)
  out <- if (NROW(raw) == 0) {
    tibble()
  } else {
    raw %>% mutate(PlayerName = str_squish(PlayerName), Season = season)
  }
  
  save_with_stamp(out, path)
  out
}

safe_logs <- function(fn, playerid, year, label) {
  tryCatch(
    fn(playerid = playerid, year = year),
    error = function(e) {
      message(label, " failed: playerid=", playerid, " year=", year,
              " -> ", conditionMessage(e))
      tibble()
    }
  )
}

# fg_minors_pitchers <- function(startSeason,endSeason){
#   url = paste0("https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=pit&qual=0&type=0&team=0,to&season=",startSeason,"&seasonEnd=",endSeason,"&org=&ind=0&splitTeam=true")
#   fgRaw <- httr::GET(url)
#   fgData<-jsonlite::fromJSON(rawToChar(fgRaw$content)) %>%
#     mutate(PlayerName = str_squish(PlayerName))
#   return(fgData)
# }
# 
# fg_minors_batters <- function(startSeason,endSeason){
#   url = paste0("https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=bat&qual=0&type=0&team=0,to&season=",startSeason,"&seasonEnd=",endSeason,"&org=&ind=0&splitTeam=true")
#   fgRaw <- httr::GET(url)
#   fgData<-jsonlite::fromJSON(rawToChar(fgRaw$content)) %>%
#     mutate(PlayerName = str_squish(PlayerName))
#   return(fgData)
# }


# main function: 
get_game_log <- function(player_name, player_info, startSeason, endSeason) {
  
  years_to_get <- seq(startSeason, endSeason, by = 1)
  
  data <- player_info %>% 
    filter(full_name == player_name)
  
  mlbam_id <- data %>% pull(id)
  is_pitcher <- data$primary_position_name == "Pitcher"
  
  cache_dir <- "cache"
  file_path <- file.path(cache_dir, paste0("game_log_", mlbam_id, ".rds"))
  season_cap <- as.Date(paste0(get_current_season(), "-09-30"))
  
  if (file.exists(file_path)) {
    if (Sys.getenv("CACHE_ONLY") == "1") {
      message("✅ CACHE_ONLY: using ", file_path)
      return(readRDS(file_path))
    }
    stamp <- cache_stamp(file_path)
    if (!is.null(stamp)) {
      age <- as.numeric(difftime(Sys.time(), stamp, units = "hours"))
      final_for_season <- Sys.Date() > season_cap && as.Date(stamp) > season_cap
      if (age < 24 || final_for_season) {
        message("Using cached game data from ", file_path, "(", round(age, 2), " hours old)")
        return(readRDS(file_path))
      }
    }
  }
  
  # Fetch player stats
  message("🔄 Fetching new ", player_name, " data from Fangraphs...")
  stat <- if (is_pitcher) "pit" else "bat"
  
  player_stats <- map_dfr(years_to_get, \(y) {
    tryCatch(
      fg_minors_season(stat, y),
      error = function(e) {
        message("Leaderboard ", y, " failed: ", conditionMessage(e))
        tibble()
      }
    )
  })
  if (nrow(player_stats) > 0) {
    player_stats <- player_stats %>% filter(PlayerName == player_name)
  }
  
  # Fetch game logs
  game_logs <- map_dfr(
    unique(player_stats$minormasterid),
    \(player_id) {
      current_player <- player_stats %>% 
        filter(minormasterid == player_id)
      
      mlb_player_id <- current_player$playerids[1]  # Get single value
      has_mlb_experience <- mlb_player_id != player_id
      seasons_played <- intersect(years_to_get, unique(current_player$Season))
      
      map_dfr(seasons_played, \(y) {
        milb_logs <- tryCatch(if (is_pitcher) {
          fg_milb_pitcher_game_logs(player_id, year = y)
        } else {
          fg_milb_batter_game_logs(player_id, year = y)
        },
        error = function(e) {
          message("MiLB failed: ", player_id, " ", y, " -> ", conditionMessage(e))
          tibble()
        })
        
        mlb_logs <- if (has_mlb_experience) {
          tryCatch(if (is_pitcher) {
            fg_pitcher_game_logs(mlb_player_id, year = y)
          } else {
            fg_batter_game_logs(mlb_player_id, year = y)
          },
          error = function(e) {
            message("MLB failed: ", mlb_player_id, " ", y, " -> ", conditionMessage(e))
            tibble()
          })
        } 
        
        if (!is.null(mlb_logs) && nrow(mlb_logs) > 0) {
          mlb_logs <- mlb_logs %>% 
            mutate(Level = "(MLB)")
        } else {
          mlb_logs <- tibble() 
        }
        
        if (is.null(milb_logs)) milb_logs <- tibble()
    
        bind_rows(milb_logs, mlb_logs)
      })
    }
  ) %>% 
    distinct()
  
  # Handle no data case
  if (nrow(game_logs) == 0) {
    message(player_name, " has no game logs for the specified period")
    result <- tibble(
      Message = paste0(player_name, " did not play between ", startSeason, " and ", endSeason)
    )
    save_with_stamp(result, file_path)
    return(result)
  }
  
  # Save and return game logs
  save_with_stamp(game_logs, file_path)
  message("💾 Saved ", player_name, " data to ", file_path)
  return(game_logs)
}




# test <- fg_minors_batters(2025, 2025)
# tigers <- test %>%
#   filter(PlayerName == "Hao-Yu Lee")
# temp <- get_game_log("Hao-Yu Lee", player_info, 2025, 2026)
# temp <- fg_milb_pitcher_game_logs(playerid = 827734, year = 2025)
# player_stats <- fg_minors_pitchers(2025, 2025) %>% 
#   filter(PlayerName == "Wei-En Lin")
# 
# temp <- load_or_update_fangraphs("Chih-Jung Liu", player_info, "Pitcher", 2025, 2025)
# batters  <- load_or_update_fangraphs("Batter", 2025, 2025)
# 
# identical(pitchers, batters)
