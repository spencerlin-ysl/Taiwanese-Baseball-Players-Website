# Taiwan_MLB_players/
team_mapping <- read_csv("data/MLB teams minor affiliates.csv") %>% 
  pivot_longer(cols = c(DSL1, DSL2), names_to = "DSL_slot", values_to = "DSL", values_drop_na = TRUE)

milb_to_mlb <- team_mapping %>%
  pivot_longer(cols = -c(MLB_abbr, DSL_slot),
               names_to = "level",
               values_to = "milb_team") %>%
  filter(!is.na(milb_team) & milb_team != "") %>%
  dplyr::select(MLB_abbr, milb_team, level) %>%
  mutate(Level_b = paste0("(", level, ")"))

level_mapping <- tibble(
  Level = c("(MLB)", "(AAA)", "(AA)", "(A+)", "(A)", "(CPX)", "(DSL)", "(R)"),
  level_ids = c(1, 11, 12, 13, 14, 16, 16, 16)
)

get_pbp_data <- function(player_name, player_info, startSeason, endSeason) {
  
  game_log <- get_game_log(player_name, player_info, startSeason, endSeason)
  if ("Message" %in% names(game_log)) {
    return(game_log)
  }
  
  player_row <- player_info %>% 
    filter(full_name == player_name)
  mlbam_id   <- player_row$id
  is_pitcher <- player_row$primary_position_name == "Pitcher"
  
  cache_dir <- "cache"
  dir.create(cache_dir, showWarnings = FALSE)
  pbp_file_path <- file.path(cache_dir, paste0("pbp_", mlbam_id, ".rds"))
  season_cap    <- as.Date(paste0(get_current_season(), "-09-30"))
  
  # Save updated cache
  finish <- function(data) {
    data <- save_with_stamp(data, pbp_file_path)
    message("💾 Saved PBP data to ", pbp_file_path)
    data
  }
  
  cached_data <- NULL
  if (file.exists(pbp_file_path)) {
    cached_data <- readRDS(pbp_file_path)
    
    # deployed app: always trust the committed cache
    if (Sys.getenv("CACHE_ONLY") == "1") {
      message("✅ CACHE_ONLY: using ", pbp_file_path)
      return(cached_data)
    }
    
    stamp <- attr(cached_data, "fetched_at")
    if (!is.null(stamp)) {
      age <- as.numeric(difftime(Sys.time(), stamp, units = "hours"))
      final_for_season <- Sys.Date() > season_cap && as.Date(stamp) > season_cap
      if (age < 24 || final_for_season) {
        message("✅ Using cached PBP data (", round(age, 2), " hours old)")
        return(cached_data)
      }
    }
  }
  
  all_game_dates <- game_log %>%
    select(Date, Team, Level) %>% 
    distinct() %>% 
    left_join(level_mapping, by = "Level") %>% 
    arrange(Date)
  
  if (!is.null(cached_data) && nrow(cached_data) > 0) {
    cache_date <- max(as.Date(cached_data$game_date), na.rm = TRUE)
    message("🔄 Cache is stale. Fetching games from ", cache_date, " onward...")
    
    dates_to_fetch <- all_game_dates %>% 
      filter(as.Date(Date) >= cache_date)
    
    if (nrow(dates_to_fetch) == 0) {
      message("✅ No new games since last cache")
      return(finish(cached_data))
    }
  } else {
    message("🔄 No usable PBP cache. Fetching all games...")
    dates_to_fetch <- all_game_dates
  }
  
  # Fetch game PKs for the dates we need
  new_game_pks <- dates_to_fetch %>% 
    distinct(Date, level_ids) %>% 
    pmap_dfr(\(Date, level_ids) {
      tryCatch(
        mlb_game_pks(date = Date, level_ids = level_ids),
        error = function(e) {
          message("game_pks failed for ", Date, ": ", conditionMessage(e))
          tibble()
        }
      )
    })
  
  log_teams <- game_log %>%
    dplyr::select(Team, Level) %>%
    distinct()

  milb_teams <- log_teams %>% 
    left_join(milb_to_mlb, by = c("Team" = "MLB_abbr", "Level" = "Level_b")) %>%
    filter(!is.na(milb_team)) %>%
    pull(milb_team) %>%
    unique()

  taiwan_game_pks <- if (nrow(new_game_pks) == 0) {
    new_game_pks
  } else {
    new_game_pks %>%
      filter(gameType == "R", # regular season only
             status.codedGameState == "F") %>% # finished games only
      filter(teams.home.team.name %in% milb_teams |
               teams.away.team.name %in% milb_teams)
  }

  # Fetch PBP data for new games
  new_player_data <- map(taiwan_game_pks$game_pk, \(pk) {
    g <- tryCatch(mlb_pbp(game_pk = pk), error = function(e) NULL) #MLB API HERE!!!
    if (is.null(g) || nrow(g) == 0) return(NULL)
    if (is_pitcher) {
      dplyr::filter(g, matchup.pitcher.fullName == player_name)
    } else {
      dplyr::filter(g, matchup.batter.fullName == player_name)
    }
  }) %>% 
    bind_rows()

  # Combine with cached data
  player_data <- if (!is.null(cached_data) && nrow(cached_data) > 0) {
    cached_data %>% 
      filter(!game_pk %in% new_player_data$game_pk) %>% 
      bind_rows(new_player_data)
  } else {
    new_player_data
  }
  
  finish(player_data)
}


# temp <- get_pbp_data("Chih-Jung Liu", player_info)
# temp_game_log <- readRDS("Taiwan_MLB_players/cache/game_log_691907.rds")
# temp_pbp <- readRDS('Taiwan_MLB_players/cache/pbp_808207.rds')
# temp_game_log <- get_game_log("Nien-Hsi Yang", player_info)
# temp <- mlb_pbp(788615)
# temp$matchup.pitcher.fullName
# games <- mlb_game_pks(date = '2018-06-20', level_ids = c(16))
