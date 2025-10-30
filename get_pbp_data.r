# Taiwan_MLB_players/
team_mapping <- read_csv("MLB teams minor affiliates.csv") %>% 
  pivot_longer(cols = c(DSL1, DSL2), names_to = "DSL_slot", values_to = "DSL", values_drop_na = TRUE)

get_pbp_data <- function(player_name, player_info, startSeason = 2025, endSeason = 2025) {
  
  game_log <- get_game_log(player_name, player_info, startSeason, endSeason)
  
  mlbam_id <- player_info %>% 
    filter(full_name == player_name) %>% 
    pull(id)
  
  cache_dir <- "cache"
  dir.create(cache_dir, showWarnings = FALSE)
  file_path <- file.path(cache_dir, paste0("game_log_", mlbam_id, ".rds"))
  pbp_file_path <- file.path(cache_dir, paste0("pbp_", mlbam_id, ".rds"))
  
  level_mapping <- tibble(
    Level = c("(MLB)", "(AAA)", "(AA)", "(A+)", "(A)", "(CPX)", "(DSL)"),
    level_ids = c(1, 11, 12, 13, 14, 16, 16)
  )
  
  if ("Message" %in% names(game_log)) {
    return(game_log)
  }
  
  all_game_dates <- game_log %>%
    select(Date, Team, Level) %>% 
    distinct() %>% 
    left_join(level_mapping, by = "Level") %>% 
    arrange(Date)
  
  # Check if cache exists and determine which dates to fetch
  if (file.exists(pbp_file_path)) {
    file_age <- difftime(Sys.time(), file.info(pbp_file_path)$mtime, units = "hours")
    
    if (file_age < 24) {
      message("✅ Using cached PBP data (", round(file_age, 2), " hours old)")
      return(readRDS(pbp_file_path))
    }
    
    if ("Message" %in% names(game_log)) {
      return(readRDS(file_path))
    }
    
    # Cache is stale - only fetch NEW games
    message("🔄 Cache is ", round(file_age, 2), " hours old. Fetching new games only...")
    cached_data <- readRDS(file_path)
    cache_date <- max(cached_data$Date)
    
    # Only get games after the cache date
    dates_to_fetch <- all_game_dates %>% 
      filter(as.Date(Date) > as.Date(cache_date))
    
    if (nrow(dates_to_fetch) == 0) {
      message("✅ No new games since last cache")
      return(readRDS(pbp_file_path))
    }
    
    message("📅 Fetching ", nrow(dates_to_fetch), " new game date(s)...")
    
  } else {
    # No cache - fetch all games
    message("🔄 No pbp cache found. Fetching all games...")
    cached_data <- NULL
    dates_to_fetch <- all_game_dates
  }
  
  # Fetch game PKs for the dates we need
  new_game_pks <- pmap_dfr(
    list(dates_to_fetch$Date, dates_to_fetch$level_ids),
    ~mlb_game_pks(date = ..1, level_ids = ..2)
  )
  
  mlb_teams <- game_log %>%
    dplyr::select(Team, Level) %>%
    distinct()
  
  milb_to_mlb <- team_mapping %>%
    pivot_longer(cols = -c(MLB_abbr, DSL_slot),
                 names_to = "level",
                 values_to = "milb_team") %>%
    filter(!is.na(milb_team) & milb_team != "") %>%
    dplyr::select(MLB_abbr, milb_team, level) %>%
    mutate(Level_b = paste0("(", level, ")"))

  milb_teams <- mlb_teams %>% 
    left_join(milb_to_mlb, by = c("Team" = "MLB_abbr", "Level" = "Level_b")) %>%
    filter(!is.na(milb_team)) %>%
    pull(milb_team) %>%
    unique()

  taiwan_game_pks <- new_game_pks %>%
    filter(teams.home.team.name %in% milb_teams |
             teams.away.team.name %in% milb_teams | 
             teams.home.team.name %in% mlb_teams | 
             teams.away.team.name %in% mlb_teams)

  # Fetch PBP data for new games
  new_player_games <- map(
    .x = taiwan_game_pks %>%
      filter(status.codedGameState == "F") %>%
      pull(game_pk),
    ~mlb_pbp(game_pk = .x) # MLB api!!!!!!!!!!
  ) %>%
    bind_rows()

  data <- player_info %>%
    filter(full_name == player_name)

  is_pitcher <- data$primary_position_name == "Pitcher"

  if(is_pitcher) {
    new_player_data <- new_player_games %>%
      filter(matchup.pitcher.fullName == player_name)
  } else {
    new_player_data <- new_player_games %>%
      filter(matchup.batter.fullName == player_name)
  }

  # Combine with cached data
  player_data <- if (!is.null(cached_data)) {
    bind_rows(cached_data, new_player_data) %>%
      distinct()  # Remove any duplicate rows
  } else {
    new_player_data
  }

  # Save updated cache

  saveRDS(player_data, pbp_file_path)
  message("💾 Saved PBP data to ", pbp_file_path)

  return(player_data)
}


# temp <- get_pbp_data("Chih-Jung Liu", player_info)
# temp_game_log <- readRDS("Taiwan_MLB_players/cache/game_log_691907.rds")
# temp_pbp <- readRDS("Taiwan_MLB_players/cache/pbp_691907.rds")
# temp_game_log <- get_game_log("Nien-Hsi Yang", player_info)
# temp <- mlb_pbp(811449)
# temp$matchup.pitcher.fullName
# games <- mlb_game_pks(date = '2025-08-08',
#                       level_ids = c(16))