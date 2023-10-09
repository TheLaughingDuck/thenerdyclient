#' get_quote_data
#'
#' @description
#' `get_quote_data` is a function that uses `lotr`, another function in this package, and formats the data.
#'
#'
#' @param character A string indicating what character quotes should be filtered by. `get_quote_data` employs string matching, so for example entering "Frodo" will specify "Frodo baggins" *and* "Frodo Gardner".
#' @param movie A string indicating what movie quotes should be filtered by. `get_quote_data` employs string matching, so for example entering "Frodo" will specify "Frodo baggins" *and*
#'
#' @return A dataframe of quotes made by a set of characters in a set of lotr films.
#'
#' @importFrom magrittr %>%
#'
#' @export get_quote_data


get_quote_data <- function(character = "", movie = ""){
  character_name_string <- character #""
  movie_name_string <- movie #""

  char_response <- lotr("character") %>%
    rename(character_id = "_id")

  movie_response <- lotr("movie") %>%
    rename("movie_id" = "_id") %>%
    rename("title" = "name")

  # Code to create a dataframe of all quotes with associated character
  quote_response <- lotr("quote")
  quote_full_info <- quote_response %>%
    # Renaming columns
    rename("character_id" = "character") %>%
    rename("movie_id" = "movie") %>%
    select(-c("id")) %>% # Repeated in `_id` column

    # Join character, then join movie, then remove some unused columns
    left_join(y=char_response, by=join_by("character_id")) %>%
    left_join(y=movie_response, by=join_by("movie_id")) %>%
    select(-c("_id", "movie_id", "wikiUrl"))

  # Code to filter out quotes by a particular character and movie
  output <- quote_full_info %>%
    filter(str_count(string=.data$name, pattern=character_name_string) > 0) %>%
    filter(str_count(string=.data$title, pattern=movie_name_string) > 0) %>%
    select(c("name", "title", "dialog"))

  return(output)
}
