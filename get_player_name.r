get_player_name <- function() {
  cache_dir <- "cache"
  file_path <- file.path(cache_dir, paste0("player_info.rds"))
  
  if (file.exists(file_path)) {
    cached <- readRDS(file_path)
    
    if (Sys.getenv("CACHE_ONLY") == "1") {
      message("✅ CACHE ONLY: using ", file_path)
      return(cached)
    }
    
    stamp <- attr(cached, "fetched_at")
    if (!is.null(stamp)) {
      age_days <- as.numeric(difftime(Sys.time(), stamp, units = "days"))
      if (age_days <= 1) {
        message("✅ Using cached player info (", round(age_days, 2), " days old)")
        return(cached)
      }
    }
  } else {
    # No cache - fetch all games
    message("🔄 No player info cache found. Fetching all players...")
  }
  
  
  ## Find all the player links in the player list 
  player_list <- read_html("https://twbsball.dils.tku.edu.tw/index.php/臺灣旅美球員列表", encoding = "UTF-8")
  trs <- player_list %>%
    html_elements("table tr")

  start_idx <- which(
    html_text2(trs) == "現役"
  )
  start_idx <- start_idx[1]
  
  end_idx <- which(
    grepl("line-height:1.25em; color:#FFFFFF; background-color:#000080;", html_attr(trs, "style"))
  )
  end_idx <- end_idx[2]
  
  active_trs <- trs[(start_idx + 1):(end_idx - 1)]
  
  get_player_link_and_year <- function(trs) {
    current_year <- NA_character_
    
    results <- map(trs, function(tr) {
      tds <- tr %>% html_elements("td")
      
      if (length(tds) == 9) {
        current_year <<- tds[1] %>% html_text2() %>% gsub("年$", "", .)
        link <- tds[2] %>% html_element("a") %>% html_attr("href")
        
      } else if (length(tds) == 8) {
        link <- tds[1] %>% html_element("a") %>% html_attr("href")
        
      } else {
        return(NULL)
      }
      
      tibble(year = current_year, link = link)
    })
    
    bind_rows(results)
  }
  
  player_links <- get_player_link_and_year(active_trs)
  
  mandarin_links <- URLdecode(player_links$link)
  
  
  ## get player name from link
  player_name <- data.frame(
    full_name  = character(length(player_links)),
    Mandarin = character(length(player_links)),
    signing_year = integer(length(player_links)),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(player_links)) {
    print(i)
    link <- paste0("https://twbsball.dils.tku.edu.tw", mandarin_links[i])
    
    page <- read_html(link, encoding = "UTF-8")
    
    player_name[i, 1] <- page %>%
      html_nodes(xpath = "//li[contains(., '外文譯名：') or contains(., '外文姓名：') or contains(., '英文譯名：')]") %>% 
      html_text(trim = TRUE) %>%
      # Remove any of the prefixes
      str_replace(".*外文譯名：\\s*", "") %>%
      str_replace(".*外文姓名：\\s*", "") %>%
      str_replace(".*英文譯名：\\s*", "") %>%    
      trimws() %>%
      .[1]
    
    player_name[i, 2] <- sub(".*=(.*?)(\\(.*)?$", "\\1", mandarin_links[i])
    
    player_name[i, 3] <- as.integer(player_links$year[i])
  }
  
  # ## Deal with first and last names
  # known_surnames <- c("Chen","Lin","Huang","Chang","Chiang","Wang","Wu","Liu","Tsai","Yang",
  #                     "Yeh","Su","Hsu","Cheng","Chou","Chu","Kuo","Ho","Lai","Hung",
  #                     "Kao","Pan","Tseng","Fan","Tsao","Fang","Chien","Shen","Tang",
  #                     "Yen","Tung","Tan","Weng","Ma","Sung","Tsui","Chan","Hou","Kuan",
  #                     "Lu","Lo","Lee","Li","Chi","Chao","Hsieh","Peng","Kang","Chiu",
  #                     "Chuang","Tai","Chung","Han","Hu","Chin","Kung","Tu","Tuan")
  # 
  # split_name <- function(df) {
  #   df %>%
  #     rowwise() %>%
  #     mutate(
  #       parts = list(strsplit(full_name, " ")[[1]]),
  #       hy_idx = which(str_detect(parts, "-")),
  #       n_spaces = str_count(full_name, " "),
  #       has_hyphen = any(str_detect(parts, "-")),
  #       
  #       name_first = if (has_hyphen & n_spaces > 1) {
  #         paste(parts[c(hy_idx - 1, hy_idx)], collapse = " ")
  #       } else {
  #         parts[str_detect(parts, "-")][1]
  #       },
  #       
  #       name_last = if (has_hyphen & n_spaces > 1) {
  #         paste(parts[-c(hy_idx - 1, hy_idx)], collapse = " ")
  #       } else {
  #         parts[!str_detect(parts, "-")][1]
  #       }
  #     ) %>%
  #     ungroup() %>%
  #     select(-parts, -hy_idx, -n_spaces, -has_hyphen)
  # }
  # 
  # player_name <- split_name(player_name)

  ## Deal with first and last names
  known_surnames <- c("Chen","Lin","Huang","Chang","Chiang","Wang","Wu","Liu","Tsai","Yang",
                      "Yeh","Su","Hsu","Cheng","Chou","Chu","Kuo","Ho","Lai","Hung",
                      "Kao","Pan","Tseng","Fan","Tsao","Fang","Chien","Shen","Tang",
                      "Yen","Tung","Tan","Weng","Ma","Sung","Tsui","Chan","Hou","Kuan",
                      "Lu","Lo","Lee","Li","Chi","Chao","Hsieh","Peng","Kang","Chiu",
                      "Zhuang","Tai","Chung","Han","Hu","Chin","Kung","Tu","Tuan", "Teng", 
                      "Sha", "Ko", "Liao")
  
  split_name <- function(df) {
    df %>%
      rowwise() %>%
      mutate(
        parts = list(strsplit(full_name, " ")[[1]]),
        n_parts = length(parts),
        # strip hyphens before checking against known_surnames so
        # "Chun-Hsi" vs "Chun Hsi" doesn't matter for surname matching
        first_is_sn = str_remove_all(parts[1], "-") %in% known_surnames,
        last_is_sn = str_remove_all(parts[n_parts], "-") %in% known_surnames,
        
        name_last = case_when(
          last_is_sn & !first_is_sn ~ parts[n_parts],
          first_is_sn & !last_is_sn ~ parts[1],
          last_is_sn & first_is_sn ~ parts[n_parts],
          TRUE                      ~ NA_character_  # ambiguous or unmatched -> review
        ),
        
        name_first = case_when(
          last_is_sn & !first_is_sn  ~ paste(parts[-n_parts], collapse = " "),
          first_is_sn & !last_is_sn  ~ paste(parts[-1], collapse = " "),
          first_is_sn & last_is_sn  ~ paste(parts[-n_parts], collapse = " "),
          TRUE                       ~ NA_character_
        )
      ) %>%
      ungroup() %>%
      select(-parts, -n_parts, -first_is_sn, -last_is_sn)
  }
  
  player_name <- split_name(player_name)
  
  ## Get player_info for app to use
  player_ids <- chadwick_player_lu()
  
  taiwan_player_ids <- player_ids %>%
    semi_join(
      player_name,
      by = c("name_first", "name_last")
    ) %>%
    mutate(name_full = paste(name_first, name_last))
  
  player_info <- mlb_people(taiwan_player_ids$key_mlbam) %>%
    rowwise() %>%
    mutate(
      team_info = list(get_player_team_affiliate(id)),
      team_name = team_info$team_name,
      level     = team_info$level,
      parent_org = team_info$team_parentorg
    ) %>%
    ungroup() %>% 
    mutate(level_abb = case_when(
      level == "Major League Baseball" ~ "MLB",
      level == "Triple-A"              ~ "AAA",
      level == "Double-A"              ~ "AA",
      level == "High-A"                ~ "A+",
      level == "Single-A"              ~ "A",
      level == "Rookie"                ~ "Rk",
      level == "Winter Leagues"        ~ "AFL",
      TRUE                            ~ NA_character_ 
    )) %>% 
    mutate(photo_link = ifelse(
      level == "Major League Baseball",
      paste0("https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_426,q_auto:best/v1/people/", id, "/headshot/67/current"),
      paste0("https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_180/v1/people/", id, "/headshot/milb/current")
    ))
  
  ## Organization Ranking
  get_taiwan_org_rank <- function(){
    url = "https://www.fangraphs.com/prospects/the-board/2026-prospect-list?sortcol=13&sortdir=desc&search=taiwan"
    webpage <- read_html(url)
    
    tables <- webpage %>%
      html_elements("table") %>%
      html_table()
    
    return(tables[[9]])
  }
  
  taiwan_org_rank <- get_taiwan_org_rank() %>% 
    mutate(full_name = str_squish(Name)) %>% 
    rename(org_rank = `Org Rk`) %>% 
    select(-Name)
  
  ## join data back to player_info
  player_info <- player_info %>%
    left_join(
      taiwan_org_rank %>%
        select(full_name, org_rank),
      by = "full_name"
    ) %>% 
    mutate(across(where(is.character), str_squish))
  
  debut_year <- taiwan_player_ids %>%
    transmute(id = as.integer(key_mlbam),
              debut_year = as.integer(pro_played_first)) %>%
    distinct(id, .keep_all = TRUE)
  
  player_name_to_join <- player_name %>% 
    select(name_first, Mandarin, signing_year = signing_year) %>%
    distinct(name_first, .keep_all = TRUE)
  
  player_info <- player_info %>% 
    mutate(id = as.integer(id)) %>%
    left_join(debut_year, by = "id") %>%
    left_join(player_name_to_join, 
              by = c("use_name" = "name_first"))

  tmp_path <- paste0(file_path, ".tmp")
  player_info <- save_with_stamp(player_info, tmp_path)
  file.rename(tmp_path, file_path)
  message("💾 Saved player info data to ", file_path)
  
  return(player_info)
}

# player_info <- get_player_name()
# player_info <- readRDS('/Users/spencerlin/Documents/My projects/Taiwan Baseball Players/Taiwan_MLB_players/cache/player_info.rds')
# player_info <- player_info %>% 
#   mutate(signing_year = as.integer(signing_year))
