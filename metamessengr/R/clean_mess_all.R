#' @title Clean the Messenger Data Text and Time in One Go
#'
#' @description This function cleans the content of the text that was sent for each message.
#' Specifically, it removes special characters, turns all words to lowercase,
#' removes stopwords (words such as the, and, etc.), and removes white space.
#'
#' A column is also created ("convo") which denotes who sent the message and
#' to which chat.
#'
#' Messages that have text longer than 640 characters are removed, as
#' 640 is the character limit for Messenger. Anything longer is bogus.
#'
#' The cleaned version of the text is stored in the original content vector. The
#' original message content is also retained, but stored in the
#' vector: "content_unclean".
#'
#' Additionally, it cleans up the time of the selected messenger data
#' so that it is more easily usable in R. It also adds columns for date and
#' hour for easy analysis.
#'
#' @param data The original messenger data that was selected using selection()
#'
#' @param custom_clean An optional character vector which contains personal
#' stop words. For example, if you'd like to remove "http", it could be added.
#'
#' @param timezone A character vector with the timezone of the user. Default
#' is Pacific Time.
#'
#' @return A tibble with cleaned up text and time data.
#'
#' @importFrom dplyr mutate filter as_tibble
#' @importFrom tm removeWords
#' @importFrom stringr str_replace_all str_to_lower str_squish
#' @importFrom chron chron
#' @importFrom magrittr %>%
#'
#' @export
clean_mess_all <- function(data, custom_clean=NULL, timezone="America/Los_Angeles"){
  timestamp_ms <- time <- NULL
  content <- sender <- sent_to <- NULL
  stop_words <- metamessengr::stop_words

  data <-  data %>%
    dplyr::mutate(timestamp_ms = timestamp_ms/1000,
                  timestamp_ms = base::as.POSIXct(timestamp_ms,
                                                  origin="1970-01-01",
                                                  tz=timezone),
                  date = base::as.Date(timestamp_ms),
                  time = base::strftime(timestamp_ms, format="%H:%M:%S"),
                  time = chron::chron(times=time),
                  time = base::as.numeric(time),
                  hour = time *24) %>%
    dplyr::as_tibble()

  data <-  data %>%
    dplyr::mutate(length = base::ifelse(base::nchar(content)>640, NA,
                                        base::nchar(content)),
                  content_unclean = content,
                  content = stringr::str_replace_all(content, "[^a-zA-Z0-9]", " "),
                  content = stringr::str_to_lower(content),
                  content = tm::removeWords(content, stop_words$word),
                  content = stringr::str_squish(content),
                  convo = base::paste(sender,"-",sent_to)
    ) %>%
    dplyr::filter(!base::is.na(content),
                  content != "",
                  content != "connected messenger")
  if(!base::is.null(custom_clean)) {
    data %>%
      dplyr::mutate(
        content = tm::removeWords(content, custom_clean)
      )
  }
  dplyr::as_tibble(data)
}
