#' Return table of game outcomes
#'
#' @param game_id A Unique ID
#' @param API_KEY A Key
#' @return A table of the outcomes of a game
#' @import httr
#' @import knitr
#' @import kableExtra
#'
#' @export Outcome



Outcome <- function(game_id, API_KEY) {
  base_url <- "https://api.sportradar.us/nhl/trial/v6/en/games/"
  end_url <- "/pbp.json?api_key="
  full_url <- paste0(base_url,game_id,end_url,API_KEY)
  get_plays <- GET(full_url)
  plays <- content(get_plays)
  if (http_status(get_plays)$category == "Success") {
    Home <- c(paste(plays$home$market, plays$home$name), plays$home$points)
    Away <- c(paste(plays$away$market, plays$away$name), plays$away$points)
    knitr::kable(rbind(Home, Away)) %>%
      kable_styling(bootstrap_options = "striped", full_width = F)
  } else {
    paste("Error!")}
}



