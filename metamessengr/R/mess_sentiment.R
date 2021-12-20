#' @title Find the Associated Sentiment of Each Word for Selected Messages
#'
#' @description This function adds sentiment scoring to each word of the
#' selected messenger data. This must be performed after running the mess_select
#' and clean_mess_text functions.
#'
#' @param data Messenger data selected using the mess_selection() function and
#' cleaned using the clean_mess_text() or clean_mess_all() functions.
#'
#' @param lexicon A character vector with that matches the desired input of
#' get_sentiments options c("bing", "afinn", "loughran", "nrc")). Default is
#' afinn. See the tidytext package for more details.
#'
#' @return A tibble in long format with words parsed and scored
#'
#' @importFrom tidytext unnest_tokens get_sentiments
#' @importFrom dplyr left_join
#' @importFrom magrittr %>%
#'
#' @export
mess_sentiment <-  function(data, lexicon = "afinn"){
  content <- NULL
    data %>%
       tidytext::unnest_tokens(
                 input = content,
                 output = "word",
                 token = "words") %>%
    dplyr::left_join(tidytext::get_sentiments(lexicon), by = "word")
}
