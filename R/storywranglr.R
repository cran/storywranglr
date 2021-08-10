# R CODE FOR STORYWRANGLER API PACKAGE

#' Explore Twitter trends with the Storywrangler ngrams API
#'
#' @description Storywrangler's ngrams API lets you search a large historical database of
#' Twitter data for daily usage statistics about strings of one, two, and three
#' words (1-grams, 2-grams, and 3-grams respectively).
#'
#' This function returns daily historical usage statistics for a given query
#' over data set's entire time range.
#'
#' For more details about Storywrangler, please see:
#'
#' * API documentation: <https://github.com/janeadams/storywrangler>
#' * Academic paper describing uses: <https://advances.sciencemag.org/content/7/29/eabe6534.full>
#'
#' @param query Character string with the n-gram(s) to query. One, two, or three
#'   words separated by spaces will run query that string as a 1-gram, 2-gram,
#'   or 3-gram respectively. More than three space-separated words will be
#'   treated as separate queries for individual 1-grams.
#' @param metric The measure of lexical fame to return: accepts values `rank`
#'   (default) and `freq`. *Note:* API returns both by default.
#' @param language Two-letter code for the language to search. Defaults to `en`.
#' @param rt Boolean for whether to include retweets.
#'
#' @return A tibble with the API query and response.
#' @export
#'
#' @examples
#' \dontrun{
#' # Query a simple 1-gram about the populatity of potatoes
#' result <- ngrams("potatoes")
#'
#' # Query a 2-gram about the popularity of potato chips
#' result <- ngrams("potato chips")
#'
#' # Query *four* 1-grams related to potatoes
#' # Note! If there are more than 3 words, they are all treated as 1-grams
#' result <- ngrams("potato potahto spud taters")
#' }
ngrams <- function(query,
                    metric = c("rank", "freq"),
                    language = "en",
                    rt = c(FALSE, TRUE)){

  # trying to write this package according to API best practices here
  # https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

  # make sure the query is a single character string of length > 0 that contains
  # a 1-gram, 2-gram, or 3-gram. NOTE the API seems to interpret a 4-gram as
  # four queries for 1-grams but here we'll restrict users to keep it simple.
  query <- paste(query, collapse = " ")
  # remove repeated whitespace
  query <- gsub(x = query, pattern = "\\s+", replacement = " ")

  #if (stringi::stri_count_fixed(query, " ") > 2) stop ("Please enter a 1-gram, 2-gram, or 3-gram.")
  if (nchar(query) == 0) stop ("Please supply a valid query.")

  # make sure the other parameters are valid
  metric <- match.arg(metric, metric)
  rt <- as.character(rt)
  rt <- match.arg(rt, rt)
  src <- "api" #match.arg(src, src)

  base_url <- "https://storywrangling.org/api/ngrams/"

  query_encoded <- urltools::url_encode(query)

  url <- sprintf("%s%s?metric=%s&language=%s&rt=%s&src=%s",
                 base_url,
                 query_encoded,
                 metric,
                 language,
                 rt,
                 src)

  resp <- httr::GET(url)

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json.", call. = FALSE)
  }

  resp_json <- httr::content(resp, type = "text/json", encoding = "UTF-8")

  # throw an error if we got an http error
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Storywrangler API request failed: status code %s\n%s",
        httr::status_code(resp),
        as.character(resp_json)
      ),
      call. = FALSE
    )
  }

  df <- jsonlite::fromJSON(resp_json)

  # create named character vector of metadata
  metadata <- unlist(df$meta)
  # metadata_names <- names(metadata)

  # extract all call results using a for loop
  # (tried briefly with map but this is just easier for now)
  results <- tibble::tibble()
  for (i in 1:length(df$data)){
    x <- df$data[[i]]
    x$date <- as.Date(x$date)
    x$query <- names(df$data)[[i]]
    x$language <- metadata["language"]
    results <- dplyr::bind_rows(results,
                                tibble::as_tibble(x))
  }

  # also give it class "storywrangler" for customized printing
  # note! this isn't implemented right now; the class doesn't do anything.
  class(results) <- c("storywrangler.ngrams", class(results))



  # store the url call in metadata attributes for printing
  attr(results, "api_call") <- url

  return (results)
}


#'Explore Twitter trends with the Storywrangler zipf API
#'
#' @description Storywrangler's ngrams API lets you search a large historical database of
#' Twitter data for daily usage statistics about strings of one, two, and three
#' words (1-grams, 2-grams, and 3-grams respectively).
#'
#' This function will query the API for a specific date to return the rank and
#'frequency data for its top n ngrams. Please note that queries of over 1000
#'ngrams will take a long time to load.
#'
#' For more details about Storywrangler, please see:
#'
#' * API documentation: <https://github.com/janeadams/storywrangler>
#' * Academic paper describing uses: <https://advances.sciencemag.org/content/7/29/eabe6534.full>
#'
#'@param date The date to query, in either character "YYYY-MM-DD" or Date
#'  format.
#'@param max The maximum number of ngrams to return. Defaults to 100.
#'@param language The two-letter code of the language to query. Defaults to
#'  "en".
#'@param ngrams Integer specifying the type of n-grams to return. Accepts 1, 2,
#'  and 3, and defaults to 1.
#'
#'@return A tibble with the API query and response.
#'@export
#'
#' @examples
#' \dontrun{
#' # Get top English 2-grams for January 6, 2021
#'  result <- zipf("2021-01-06", ngrams = 2)
#' }
zipf <- function(date,
                 max = 100,
                 language = "en",
                 ngrams = c(1,2,3)) {

  base_url <- "https://storywrangling.org/api/zipf/"

  # make sure the date parses properly and is up to 2 days prior to today
  tryCatch(expr = {date_check <- as.Date(date, tryFormats = "%Y-%m-%d")},
           error = function(e) message("Error parsing date. Please use format YYYY-MM-DD."))

  if (date_check + 2 >= Sys.Date() ) stop ("Please enter a date up to two days prior to today in format YYYY-MM-DD.")

  # make sure max is numeric
  if (!is.numeric(max)) stop ("Please supply a numeric value for parameter max.")
  # then round it down (in case someone gives a weird value) and make it a char
  max <- as.character(floor(max))

  # make sure ngrams match and are a character
  ngrams <- as.character(ngrams)
  ngrams <- match.arg(ngrams, ngrams)

  url <- sprintf("%s%s?max=%s&language=%s&n=%s",
                 base_url,
                 date,
                 max,
                 language,
                 ngrams)


  resp <- httr::GET(url)

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json.", call. = FALSE)
  }

  resp_json <- httr::content(resp, type = "text/json", encoding = "UTF-8")

  # throw an error if we got an http error
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Storywrangler zipf API request failed: status code %s\n%s",
        httr::status_code(resp),
        as.character(resp_json)
      ),
      call. = FALSE
    )
  }

  # parse the results from the json
  df <- jsonlite::fromJSON(resp_json)

  # put the data into a tibble
  data <- tibble::as_tibble(df$data)

  # add the date as a column for completeness
  data$date <- date

  # also give it class "storywrangler.zipf" for customized printing
  # note! this isn't implemented right now; the class doesn't do anything.
  class(data) <- c("storywrangler.zipf", class(data))

  # create named character vector of metadata
  metadata <- unlist(df$meta)
  metadata_names <- names(metadata)

  # store the metadata in attributes for printing
  attr(data, "api_call") <- url
  for (i in 1:length(metadata_names)){
    attr(data, metadata_names[[i]]) <- as.character(metadata[[i]])
  }

  return (data)

}





# tbl_sum.storywrangler.ngrams <- function(x, ...){
#   default_header <- NextMethod()
#   c(default_header,
#     "Storywrangler ngrams API call" = attr(x, "api_call")
#     # ,
#     # query = attr(x, "query"),
#     # language = attr(x, "language"),
#     # response = attr(x, "response"),
#     # gapped = attr(x, "gapped"),
#     # viewer = attr(x, "viewer")
#   )
# }
#
# tbl_sum.storywrangler.zipf <- function(x, ...){
#   default_header <- NextMethod()
#   c(default_header,
#     "Storywrangler zipf API call" = attr(x, "api_call"),
#     query = attr(x, "query"),
#     language = attr(x, "language"),
#     response = attr(x, "response"),
#     gapped = attr(x, "gapped"),
#     viewer = attr(x, "viewer"),
#     n = attr(x, "n")
#   )
# }
