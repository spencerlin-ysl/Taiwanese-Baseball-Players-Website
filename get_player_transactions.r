library(rvest)
library(dplyr)

get_player_transactions <- function(mlbam) {
  link <- paste0("https://www.milb.com/player/", mlbam)
  
  page <- read_html(link, encoding = "UTF-8")
  
  transactions <- page %>% 
    html_nodes(xpath = '//*[@id="transactions"]/h2 | //*[@id="transactions"]/div[1]') %>% 
    as.character() %>%
    paste(collapse = "")
    
  return(transactions)
}

# temp <- get_player_transactions(813820)
# temp
