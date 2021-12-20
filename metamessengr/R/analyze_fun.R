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

}

#' @title Plot Sentiment Analysis Over Time per Sender
#'
#' @description This function creates ggplots of Messenger data that have been
#' run through the get_mess_sentiment function.
#'
#' @param data The original messenger data that was selected using selection()
#'
#' @return A ggplot column-plot of the sentiment before and after an event,
#' and a density plot of each sender's overall sentiment before and after the
#' event date specified.
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap geom_density vars
#'
#' @export
plot_mess <- function(data) {
  timestamp_ms <- value <- event <- vars <- sender <- NULL

  event_ts_p =
    ggplot2::ggplot(data,
               ggplot2::aes(x= timestamp_ms, y = value, fill = event, color = event )) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::facet_wrap(ggplot2::vars(sender))

  sentiment_hist =
    ggplot2::ggplot(data,
                    ggplot2::aes(value, fill= event, color = event)) +
    ggplot2::geom_density(position = "dodge", na.rm = T, alpha = .5) +
    ggplot2::facet_wrap(ggplot2::vars(sender))

}

