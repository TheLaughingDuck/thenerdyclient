#' lotr()
#'
#' @description A function for connection with the LOTR API. There is a rate limit: 100 requests per 10 min, or one every 6 seconds.
#'
#' @param end_point_argument A string (one of "character", "movie", "book", "quotes" etc) that specifies the end point of the lotr API that should be called in the GET request.
#'
#' @return A dataframe of the data that was returned from the API.
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


lotr <- function(end_point_argument = "movie"){

  # ---V--- CHECK INPUT ---V---
  # Check arg1 argument
  #stopifnot("argument \"arg1\" is not ---" = is.---(arg1))

  # Check arg2 argument
  #stopifnot("argument \"arg2\" is not ---" = is.---(arg2))
  # ---^--- CHECK INPUT ---^---

  base_url <- "https://the-one-api.dev/v2"
  end_point <- paste0("/", end_point_argument) #"/quote"
  query_url <- paste0(base_url, end_point)

  # Get request using temporary access token
  response <- GET(query_url, add_headers(Authorization="Bearer GBwKnNGIpaO26KR3LNlc"))
  #response <- GET(query_url, add_headers(Authorization=paste("Bearer", Sys.getenv("lotr_id"))))

  # Check status code
  if(response$status_code == 401){print("Status code 401: Unauthorized. The request lacks valid authentication.")}
  else if(response$status_code == 404){print("Status code 404: could not find a client-requested webpage")}
  else if(response$status_code == 429){print("Status code 429: Too many requests. You have been rate limited.")}
  else if(response$status_code == 200){print("Status code 200: The request was OK/successsful.")}
  else {cat("Unknown status code: ", response$status_code)}

  # Format the docs in response as a tibble
  response <- fromJSON(content(response, as="text"))$docs %>% as_tibble()
  #response_data <- as_tibble(response$docs)

  # Return the response to the user
  return(response)
}

