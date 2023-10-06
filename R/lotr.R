#' LOTR
#'
#' @description A function for connection with the LOTR API.
#'
#' @param arg1 A short description of the arg1 argument. Must specify: accepted format.
#'
#' @param arg2 A short description of the arg2 argument. Must specify: accepted format.
#'
#' @return A short description of the output format.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter as_tibble rename select left_join join_by
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_count
#' @importFrom rlang .data
#'
#' @source [The one API](https://the-one-api.dev/)
#'
#' @export lotr


lotr <- function(arg1 = "character", arg2){

  # ---V--- CHECK INPUT ---V---
  # Check arg1 argument
  #stopifnot("argument \"arg1\" is not ---" = is.---(arg1))

  # Check arg2 argument
  #stopifnot("argument \"arg2\" is not ---" = is.---(arg2))
  # ---^--- CHECK INPUT ---^---

  base_url <- "https://the-one-api.dev/v2"
  end_point <- paste0("/", arg1) #"/quote"
  query_url <- paste0(base_url, end_point)
  response <- GET(query_url, add_headers(Authorization=paste("Bearer", Sys.getenv("lotr_id"))))

  if(response$status_code == 401){print("Status code 401: Unauthorized. The request lacks valid authentication.")}
  else if(response$status_code == 404){print("Status code 404: could not find a client-requested webpage")}
  else if(response$status_code == 429){print("Status code 429: Too many requests. You have been rate limited.")}
  else if(response$status_code == 200){print("Status code 200: The request was OK/successsful.")}
  else {cat("Unknown status code: ", response$status_code)}

  # Format the response
  response <- fromJSON(content(response, as="text"))

  response_data <- as_tibble(response$docs)

  #View(response)

  # Return the response to the user
  return(response_data)
}

# The one API rate limit: 100 requests per 10 min, or one every 6 seconds.

#response <- lotr()


get_shiny_data <- function(character = "", movie = ""){
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
