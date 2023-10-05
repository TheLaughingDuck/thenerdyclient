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
#' @importFrom dplyr arrange filter as_tibble
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#'
#' @source [The one API](https://the-one-api.dev/)
#'
#' @export lotr


lotr <- function(arg1, arg2){

  # ---V--- CHECK INPUT ---V---
  # Check arg1 argument
  #stopifnot("argument \"arg1\" is not ---" = is.---(arg1))

  # Check arg2 argument
  #stopifnot("argument \"arg2\" is not ---" = is.---(arg2))
  # ---^--- CHECK INPUT ---^---

  base_url <- "https://the-one-api.dev/v2"
  end_point <- "/character"
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



