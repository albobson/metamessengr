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
#' @return A data frame with cleaned up text data.
#'
#' @export
clean_mess_text <- function(data){
  tidy_text <- data %>%
    mutate(length = ifelse(nchar(content)>640, NA, nchar(content)),
           content_unclean = content,
           content = str_replace_all(content, "[^a-zA-Z0-9]", " "),
           content = str_to_lower(content),
           content = removeWords(content, stop_words$word),
           content = str_squish(content),
           convo = paste(sender,"-",sent_to)
    ) %>%
    filter(!is.na(content),
           content != "",
           content != "connected messenger")
}
