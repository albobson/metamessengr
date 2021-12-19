#' @title Sender Summary
#'
#' @param data A cleaned data frame of messenger data
#'
#' @return A tibble summarizing the total number of messages and the total
#' number of characters sent by each person, to each specific chat
#' in the data set.
#'
#' @import dplyr
#'
#' @export
sender_sum <- function(data) {
  data %>%
  group_by(sender,sent_to) %>%
  summarise(count = n(),
            length = sum(length, na.rm = T))
}

#' @title Group Chat Message Summary
#'
#' @param data A cleaned data frame of messenger data
#'
#' @return A tibble summarizing the total number of messages and the total
#' number of characters sent in a specific group chat in the dataset.
#'
#' @import dplyr
#'
#' @export
group_sum <- function(data) {
  data %>%
    group_by(sent_to) %>%
    summarise(count = n(),
              length = sum(length, na.rm = T))
}

#' @title Top Words By User
#'
#' @param data A cleaned data frame of messenger data
#'
#' @return A tibble of the number of dictionary words used by each person and
#' the total number of times used.
#'
#' @import dplyr stats tidyr
#'
#' @export
top_words <- function(data) {
  data("grady_augmented")
  grady_augmented=as_tibble(grady_augmented)
  grady_augmented=rename(grady_augmented, words=value)
  data = data %>%
    unnest_tokens(input = content,
                  output = "words",
                  token = "words") %>%
    semi_join(grady_augmented) %>%
    group_by(sender, words) %>%
    summarise(used = n()) %>%
    ungroup() %>%
    mutate(sender = ifelse(sender=="",NA,sender)) %>%
    na.omit()
  data %>%
    group_by(sender) %>%
    arrange(desc(used), .by_group = TRUE)
}
