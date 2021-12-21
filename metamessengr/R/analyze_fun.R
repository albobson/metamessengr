#' @title Run T Test on Messenger Data With Date Cut-Off Described.
#'
#' @description This function runs a t.test on messenger data that has been
#' cleaned and has been run through both mess_cut_off() and get_mess_sentiment()
#' It runs a t.test on the sentiment before and after the event time specified
#' in order to determine if there was a difference in word sentiment after the
#' event time.
#'
#' @param data Messenger data that has been cleaned using either the
#' clean_mess_text() and clean_mess_time() functions, or cleaned using the
#' clean_mess_all() function. Additionally, it must have been run through the
#' mess_cut_off() and the mess_sentiment() functions
#'
#' @return A tibble in long format filtered by top users
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr %>%
#' @importFrom stats t.test
#'
#' @export
t_mess <-  function(data){
  event <- value <- NULL
  x = data %>%
    dplyr::filter(event == "pre") %>%
    dplyr::select(value)

  y = data %>%
    dplyr::filter(event == "post") %>%
    dplyr::select(value)

  t_test = stats::t.test(x,y)
  t_test

}

#' @title Plot Sentiment Analysis Over Time
#'
#' @description This function creates ggplots of Messenger data that have been
#' run through get_mess_sentiment() and through mess_cut_off(). Facet wrapping
#' is applied tomake multiple plots, with "sender" being the default variable
#' to wrap.
#'
#' @param data The original messenger data that was selected using selection()
#'
#' @return A ggplot column-plot of the sentiment before and after an event,
#' and a density plot of each sender's overall sentiment before and after the
#' event date specified.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap geom_density vars
#'
#' @export
plot_senti_time <- function(data) {
  timestamp_ms <- value <- event <- vars <- sender <- NULL

  event_ts_p =
    ggplot2::ggplot(data,
               ggplot2::aes(x= timestamp_ms, y = value, fill = event, color = event )) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::facet_wrap(ggplot2::vars(sender))

  event_ts_p
}

#' @title Plot Sentiment Analysis Over Time per Sender
#'
#' @description This function creates ggplots of Messenger data that have been
#' run through the get_mess_sentiment function.
#'
#' @param data The original messenger data that was selected using selection()
#' @param wrap_by The variable by which the ggplot will facet wrap
#'
#' @return A ggplot density plot of each sender's overall sentiment before and
#' after the event date specified.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap geom_density vars
#'
#' @export
plot_mess_dens <- function(data){
  value <- event <- vars <- sender <- NULL
  sentiment_hist =
  ggplot2::ggplot(data,
                  ggplot2::aes(value, fill= event, color = event)) +
  ggplot2::geom_density(position = "dodge", na.rm = T, alpha = .5) +
  ggplot2::facet_wrap(ggplot2::vars(sender))

sentiment_hist
}
