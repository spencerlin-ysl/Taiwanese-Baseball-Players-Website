library(httr)
library(jsonlite)
library(dplyr)
library(rlang)

get_player_team_affiliate <- function(mlbam_id) {
  url <- paste0("https://statsapi.mlb.com/api/v1/people/", mlbam_id, "?hydrate=currentTeam,team,stats(type=[yearByYear,yearByYearAdvanced,careerRegularSeason,careerAdvanced,availableStats](team(league)),leagueListId=mlb_milb)&site=en")
  
  res <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))
  
  if (!is.null(res$people$currentTeam)) {
    data.frame(
      player_id = mlbam_id,
      team_id = res$people$currentTeam$id,
      team_name = res$people$currentTeam$name,
      team_abbrev = res$people$currentTeam$abbreviation,
      team_parentorg = res$people$currentTeam$parentOrgName %||% NA,
      level = res$people$currentTeam$sport$name,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      player_id = mlbam_id,
      team_id = NA,
      team_name = NA,
      team_abbrev = NA,
      team_parentorg = NA,
      level = NA,
      stringsAsFactors = FALSE
    )
  }
}
get_player_team_affiliate(801179)
