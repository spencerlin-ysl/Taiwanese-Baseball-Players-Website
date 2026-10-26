# refresh.R
# Run by .github/workflows/refresh.yml (or manually: Rscript refresh.R)
# Rebuilds / updates everything in cache/ so the deployed app can run with CACHE_ONLY=1.

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(xml2)
  library(rlang)
  library(jsonlite)
  library(httr)
  library(glue)
  library(baseballr)
})

# Never run the refresh in cache-only mode
Sys.setenv(CACHE_ONLY = "0")

# Same order as app.R
source("mlb_pbp_new.R")
source("get_player_team_affiliate.r")
source("get_game_log.r")
source("get_pbp_data.r")
source("get_player_name.r")

started    <- Sys.time()
budget_min <- as.numeric(Sys.getenv("RUN_BUDGET_MINUTES", "300"))
season     <- get_current_season()

elapsed_min <- function() as.numeric(difftime(Sys.time(), started, units = "mins"))

# ---- 1. player list ---------------------------------------------------------
# Rebuild player_info if it is older than 12 hours. This avoids the
# "stamped 9:05, checked 9:00 next day" problem with the 1-day check.
player_info_path <- file.path("cache", "player_info.rds")
if (cache_age_hours(player_info_path) > 12) {
  unlink(player_info_path)
}

player_info <- get_player_name()
message("Players: ", nrow(player_info))

# ---- 2. order: players with no / oldest cache first ------------------------
queue <- player_info %>%
  mutate(
    debut_year_safe = suppressWarnings(as.integer(debut_year)),
    debut_year_safe = if_else(is.na(debut_year_safe), as.integer(season), debut_year_safe),
    log_age = map_dbl(id, \(i) cache_age_hours(file.path("cache", paste0("game_log_", i, ".rds"))))
  ) %>%
  arrange(desc(log_age))   # Inf (no cache or no stamp) comes first

# ---- 3. loop ----------------------------------------------------------------
results <- list()

for (i in seq_len(nrow(queue))) {
  nm <- queue$full_name[i]
  yr <- queue$debut_year_safe[i]
  
  if (elapsed_min() > budget_min) {
    message("⏱️ Time budget (", budget_min, " min) reached after ", i - 1,
            " players. Remaining players will be handled next run.")
    break
  }
  
  message("[", i, "/", nrow(queue), "] ", nm, " (from ", yr, ")")
  
  results[[nm]] <- tryCatch({
    get_pbp_data(nm, player_info, yr, season)   # calls get_game_log() internally
    "ok"
  }, error = function(e) {
    message("⚠️ ", nm, " failed: ", conditionMessage(e))
    paste("failed:", conditionMessage(e))
  })
  
  Sys.sleep(1)   # be polite to FanGraphs / the MLB API
}

# ---- 4. summary -------------------------------------------------------------
status <- unlist(results)
message("\n==== Refresh summary ====")
message("Processed: ", length(status), " of ", nrow(queue))
message("OK:        ", sum(status == "ok"))
message("Failed:    ", sum(status != "ok"))
if (any(status != "ok")) {
  message("\nFailed players:")
  walk2(names(status)[status != "ok"], status[status != "ok"],
        \(n, s) message(" - ", n, ": ", s))
}
message("Elapsed: ", round(elapsed_min(), 1), " min")

# Only fail the job if nothing worked at all (so partial progress still gets committed)
if (length(status) > 0 && all(status != "ok")) {
  stop("Every player failed. Check the log above (likely a FanGraphs/MLB API block).")
}