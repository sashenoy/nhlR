#' Return a table of game-ids and home vs away info for a given date
#'
#' @param date A Date formatted as YYYY/MM/DD
#' @param API_KEY A Key
#' @return A table of the daily schedule
#' @import dplyr
#' @import httr
#' @import kableExtra
#' @export DailySchedule


DailySchedule <- function(date, API_KEY) {
  #format date as YYYY/MM/DD
  base_url <- "http://api.sportradar.us/nhl/trial/v6/en/games/"
  end_url <- "/schedule.json?api_key="
  full_url <- paste0(base_url, date, end_url, API_KEY)
  get_schedule <- GET(full_url)
  daily <- content(get_schedule)
  if (http_status(get_schedule)$category == "Success") {
    games <- daily$games
    games_list <- data.frame(a = 1:length(games))
    games_list$a <- NULL
    games_list$id <- unlist(sapply(games, '[[', "id"))
    games_list$teams <- unlist(sapply(games, function(x) paste(x$home$name, "vs.", x$away$name)))
    games_list$points <- unlist(sapply(games, function(x) paste(x$home_points, "-", x$away_points)))

    games_list %>%
      knitr::kable(caption = paste(date, "Games")) %>%
      kable_styling(bootstrap_options = "striped", full_width = F)
  } else {
    print("Error!") } }
