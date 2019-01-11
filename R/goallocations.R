#' Return an interactive coordinate plane with locations of goals for a given game
#'
#' @name GoalLocations
#' @param game_id A Unique ID
#' @param API_KEY A Key
#' @return A plane with points plotting x and y coordinates of a goal
#' @import dplyr
#' @import httr
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom ggthemes theme_few
#' @importFrom utils globalVariables
#' @export GoalLocations
#'
globalVariables(c("coord_x", "coord_y", "description", "team"))

GoalLocations <- function(game_id, API_KEY) {
  globalVariables(c("coord_x", "coord_y", "description", "team"))
  base_url <- "https://api.sportradar.us/nhl/trial/v6/en/games/"
  end_url <- "/pbp.json?api_key="
  full_url <- paste0(base_url,game_id,end_url,API_KEY)
  get_plays <- GET(full_url)
  plays <- content(get_plays)
  if (http_status(get_plays)$category == "Success") {
    Home <- paste(plays$home$market, plays$home$name)
    Away <- paste(plays$away$market, plays$away$name)

    events <- lapply(plays[["periods"]], '[[', 'events')
    goals <- sapply(events, function(x) x[c(which(sapply(x, function(y) "goal" %in% y)))])

    if (length(goals) == 4) {
      df1 <- as.data.frame(t((sapply(goals[[1]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[1]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[1]], function(x) paste(x$attribution$market,x$attribution$name)))

      df2 <- as.data.frame(t((sapply(goals[[2]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[2]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[2]], function(x) paste(x$attribution$market,x$attribution$name)))

      df3 <- as.data.frame(t((sapply(goals[[3]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[3]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[3]], function(x) paste(x$attribution$market,x$attribution$name)))

      df4 <- as.data.frame(t((sapply(goals[[4]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[4]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[4]], function(x) paste(x$attribution$market,x$attribution$name)))

      mass_df <- rbind(df1, df2, df3, df4)
      mass_df$coord_x <- unlist(mass_df$coord_x)
      mass_df$coord_y <- unlist(mass_df$coord_y)
    } else {

      df1 <- as.data.frame(t((sapply(goals[[1]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[1]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[1]], function(x) paste(x$attribution$market,x$attribution$name)))

      df2 <- as.data.frame(t((sapply(goals[[2]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[2]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[2]], function(x) paste(x$attribution$market,x$attribution$name)))

      df3 <- as.data.frame(t((sapply(goals[[3]], function(x) x$location)))) %>%
        mutate(description = sapply(goals[[3]], function(x) x$description)) %>%
        mutate(team = sapply(goals[[3]], function(x) paste(x$attribution$market,x$attribution$name)))

      mass_df <- rbind(df1, df2, df3)
      mass_df$coord_x <- unlist(mass_df$coord_x)
      mass_df$coord_y <- unlist(mass_df$coord_y)
    }
    mass_df %>% ggplot(aes(x = coord_x, y = coord_y, text = description, color = team)) + geom_jitter() + theme_few() + labs(x="", y="", title = paste(Home, "vs.", Away, "Goals"), caption = "Hockey Rink Coordinate Plane") + scale_x_continuous(breaks = c()) +
      scale_y_continuous(breaks = c()) -> p

    return(ggplotly(p, tooltip = "description"))
  } else {
    paste("Error!")
  }
}
