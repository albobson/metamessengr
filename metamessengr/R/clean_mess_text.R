#' @title Clean the messenger data text content
#'
#' This function cleans the content of the text that was sent for each message.
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
#' @param data The original messenger data that was selected using selection()
#'
#' @param custom_clean An optional character vector which contains personal
#' stop words. For example, if you'd like to remove "http", it could be added.
#'
#' @return A data frame with cleaned up text data.
#'
#' @importFrom dplyr mutate filter
#' @importFrom tm removeWords
#' @importFrom stringr str_replace_all str_to_lower str_squish
#' @importFrom magrittr %>%
#'
#' @export
clean_mess_text <- function(data, custom_clean=NULL) {
  stop_words <- data(stop_words)
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
data
}
