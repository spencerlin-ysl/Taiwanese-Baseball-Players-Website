fg_minors_pitchers <- function(startSeason,endSeason){
  url = paste0("https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=pit&qual=0&type=0&team=0,to&season=",startSeason,"&seasonEnd=",endSeason,"&org=&ind=0&splitTeam=true")
  fgRaw <- httr::GET(url)
  fgData<-jsonlite::fromJSON(rawToChar(fgRaw$content)) %>%
    mutate(PlayerName = str_squish(PlayerName))
  return(fgData)
}

fg_minors_batters <- function(startSeason,endSeason){
  url = paste0("https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=bat&qual=0&type=0&team=0,to&season=",startSeason,"&seasonEnd=",endSeason,"&org=&ind=0&splitTeam=true")
  fgRaw <- httr::GET(url)
  fgData<-jsonlite::fromJSON(rawToChar(fgRaw$content)) %>%
    mutate(PlayerName = str_squish(PlayerName))
  return(fgData)
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


get_game_log <- function(player_name, player_info, startSeason, endSeason) {
  
  years_to_get <- seq(startSeason, endSeason, by = 1)
  
  data <- player_info %>% 
    filter(full_name == player_name)
  
  mlbam_id <- data %>% pull(id)
  is_pitcher <- data$primary_position_name == "Pitcher"
  
  cache_dir <- "cache"
  file_path <- file.path(cache_dir, paste0("game_log_", mlbam_id, ".rds"))
  
  # Check if cached file exists and is fresh
  if (file.exists(file_path)) {
    file_age <- difftime(Sys.time(), file.info(file_path)$mtime, units = "hours")
    month_day <- format(Sys.time(), "%m-%d")
    if (file_age < 24) { # | (month_day >= "10-01" | month_day <= "03-15")
      message("✅ Using cached game log data from ", file_path, "(", round(file_age, 2), " hours old)")
      return(readRDS(file_path))
    }
  }
  
  # Fetch player stats
  message("🔄 Fetching new ", player_name, " data from Fangraphs...")
  if(is_pitcher) {
    player_stats <- fg_minors_pitchers(startSeason, endSeason) %>% 
      filter(PlayerName == player_name)
  } else {
    player_stats <- fg_minors_batters(startSeason, endSeason) %>% 
      filter(PlayerName == player_name)
  }
  
  # Fetch game logs
  game_logs <- map_dfr(
    unique(player_stats$minormasterid),
    \(player_id) {
      map_dfr(years_to_get, \(y) {
        
        current_player <- player_stats %>% 
          filter(minormasterid == player_id)
        
        mlb_player_id <- current_player$playerids[1]  # Get single value
        has_mlb_experience <- mlb_player_id != player_id
        
        milb_logs <- if (is_pitcher) {
          fg_milb_pitcher_game_logs(playerid = player_id, year = y)
        } else {
          fg_milb_batter_game_logs(playerid = player_id, year = y)
        }
        
        mlb_logs <- if (has_mlb_experience) {
          if (is_pitcher) {
            fg_pitcher_game_logs(mlb_player_id, year = y)
          } else {
            fg_batter_game_logs(mlb_player_id, year = y)
          }
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
    saveRDS(result, file_path)
    message("💾 Saved 'no data' message to ", file_path)
    return(result)
  }
  
  # Save and return game logs
  saveRDS(game_logs, file_path)
  message("💾 Saved ", player_name, " data to ", file_path)
  return(game_logs)
}




# test <- fg_minors_batters(2025, 2025)
# tigers <- test %>%
#   filter(PlayerName == "Hao-Yu Lee")
# temp <- get_game_log("Wei-En Lin", player_info)
# temp <- get_game_log("Hao-Yu Lee", player_info, 2025, 2026)
# temp <- fg_milb_pitcher_game_logs(playerid = 827734, year = 2025)
# player_stats <- fg_minors_pitchers(2025, 2025) %>% 
#   filter(PlayerName == "Wei-En Lin")
# 
# temp <- load_or_update_fangraphs("Chih-Jung Liu", player_info, "Pitcher", 2025, 2025)
# batters  <- load_or_update_fangraphs("Batter", 2025, 2025)
# 
# identical(pitchers, batters)
