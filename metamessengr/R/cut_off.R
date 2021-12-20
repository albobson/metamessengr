#' @title Describe messages as "pre" or "post" an event
#'
#' @description This function adds an event variable as "pre" or "post" a time set by user
#' to be used to measure for statistical tests on Facebook Messenger data after using the
#' selection() and clean_mess_time().
#'
#' @param data The original messenger data that was selected using selection()
#' and cleaned using clean_mess_time() or clean_mess_all().
#'
#' @param time A character vector with the selected date (for
#' example: "2000-12-31)
#'
#'
#' @return A tibble in long format with messages described as "pre" or "post"
#' an event.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
mess_cut_off <-  function(data, time) {
  data %>%
    dplyr::mutate(event = base::ifelse(date < base::as.Date(time, origin = "1970-01-01"),
                          "pre", #if before cutoff time
                          "post")) #if before cutoff time
}
