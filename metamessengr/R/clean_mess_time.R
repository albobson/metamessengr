#' @title Clean selected Facebook Messenger data
#'
#' This function cleans up the selected Facebook Messenger data after using the
#' selection() function.
#'
#' This will convert the time-stamp to a usable format.
#'
#' @param data The original messenger data that was selected using selection()
#'
#' @return A data frame with time stamp fixed to be usable in R, and date and
#' time columns.
#'
#' @export
clean_mess_time <- function(data) {
  ret = data %>%
    mutate(timestamp_ms = timestamp_ms/1000,
           timestamp_ms = as.POSIXct(timestamp_ms,
                                     origin="1970-01-01",
                                     tz="America/Los_Angeles"),
           date = as.Date(timestamp_ms),
           time = strftime(timestamp_ms, format="%H:%M:%S"),
           time = chron(times=time),
           time = as.numeric(time),
           hour = time *24)
  ret
}
