#' @title Clean selected Facebook Messenger data
#'
#' @description This function cleans up the selected Facebook Messenger data after using the
#' selection() function.
#'
#' This will convert the time-stamp to a usable format.
#'
#' @param data The original messenger data that was selected using selection()
#' @param timezone A character vector with the timezone of the user. Default
#' is Pacific Time.
#'
#' @return A tibble with time stamp fixed to be usable in R, and date and
#' time columns.
#'
#' @importFrom dplyr mutate as_tibble
#' @importFrom chron chron
#' @importFrom magrittr %>%
#'
#' @export
clean_mess_time <- function(data, timezone="America/Los_Angeles") {
  timestamp_ms <- time <- NULL
  data %>%
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
}
