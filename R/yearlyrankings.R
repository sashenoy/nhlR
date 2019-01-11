#' Return table of the yearly rankings, divided by conference and division for a given year
#'
#' @param season_year A Year
#' @param API_KEY A Key
#' @return A table of the outcomes of a game
#' @import httr
#' @import kableExtra
#' @importFrom rowr cbind.fill
#' @importFrom tibble add_column
#' @export YearlyRankings


#' @describeIn YearlyRankings returns table
YearlyRankings <- function(season_year, API_KEY){
  base_url <- "https://api.sportradar.us/nhl/trial/v6/en/seasons/"
  end_url <- '/REG/rankings.json?api_key='
  full_url <- paste0(base_url, season_year,end_url, API_KEY)
  get_rankings <- GET(full_url)
  if (http_status(get_rankings)$category == "Success") {
    rankings <- content(get_rankings)
    eastern <- lapply(rankings$conferences[[1]]$divisions, '[[', 'teams')
    western <- lapply(rankings$conferences[[2]]$divisions, '[[', 'teams')
    atlantic_rankings <- sapply(eastern[[1]], function(x) paste(x$market, x$name)) %>%
      as.data.frame()
    metropolitan_rankings <- sapply(eastern[[2]], function(x) paste(x$market, x$name)) %>%
      as.data.frame()
    central_rankings <- sapply(western[[1]], function(x) paste(x$market, x$name)) %>%
      as.data.frame()
    pacific_rankings <- sapply(western[[2]], function(x) paste(x$market, x$name)) %>%
      as.data.frame()
    rankings_table <- rowr::cbind.fill(atlantic_rankings, metropolitan_rankings, central_rankings, pacific_rankings, fill = NA) %>% add_column(id = c(1:8), .before = 1)
    colnames(rankings_table) <- c("Rank", "Atlantic Division", "Metropolitan Division", "Central Division", "Pacific Division")
    knitr::kable(rankings_table, caption = paste(season_year, "Rankings")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    add_header_above(c("", "Eastern Conference" = 2 , "Western Conference" = 2))
  }  else {
    print("Error!")
  }
}
