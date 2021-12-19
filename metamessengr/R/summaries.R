#' @title Sender Summary
#'
#' @param data A cleaned data frame of messenger data
#'
#' @return A tibble summarizing the total number of messages and the total
#' number of characters sent by each person, to each specific chat
#' in the data set.
#'
#' @importFrom dplyr group_by summarize n
#' @importFrom magrittr %>%
#'
#' @export
sender_sum <- function(data) {
  data %>%
    dplyr::group_by(sender,sent_to) %>%
    dplyr::summarise(count = dplyr::n(),
            length = base::sum(length, na.rm = T))
}

#' @title Group Chat Message Summary
#'
#' @param data A cleaned data frame of messenger data
#'
#' @return A tibble summarizing the total number of messages and the total
#' number of characters sent in a specific group chat in the dataset.
#'
#' @importFrom dplyr group_by summarize n
#' @importFrom magrittr %>%
#'
#' @export
group_sum <- function(data) {
  data %>%
    dplyr::group_by(sent_to) %>%
    dplyr::summarise(count = dplyr::n(),
              length = base::sum(length, na.rm = T))
}

#' @title Top Words By User
#'
#' @param data A cleaned data frame of messenger data
#'
#' @return A tibble of the number of dictionary words used by each person and
#' the total number of times used.
#'
#' @importFrom dplyr as_tibble rename semi_join group_by summarise ungroup mutate arrange desc n
#' @importFrom tidytext unnest_tokens
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#'
#' @export
top_words <- function(data) {
  grady_augmented=dplyr::as_tibble(data(grady_augmented))
  grady_augmented=dplyr::rename(grady_augmented, words=value)
  data = data %>%
    tidytext::unnest_tokens(input = content,
                  output = "words",
                  token = "words") %>%
    dplyr::semi_join(grady_augmented) %>%
    dplyr::group_by(sender, words) %>%
    dplyr::summarise(used = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sender = base::ifelse(sender=="",NA,sender)) %>%
    stats::na.omit()
  data %>%
    dplyr::group_by(sender) %>%
    dplyr::arrange(dplyr::desc(used), .by_group = TRUE)
}
