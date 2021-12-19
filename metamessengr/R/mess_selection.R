#' @title Select FB Messenger Data
#'
#' This function selects raw Facebook Messenger data. All that's needed are your
#' Facebook Messenger folders unzipped. The output is a data frame of
#' every message's content, sender and time stamp.
#'
#' @param file_loc A character vector with the file path where the messenger
#' data is stored. If NULL, will use working directory.
#'
#' @return A data frame of each message's sender, content and time-stamp.
#'
#' @import dplyr purrr stringr
#'
#' @export
mess_selection <- function(file_loc) {
  if(is.null(file_loc)){
    file_loc = getwd()
  }
  wd <- getwd()
  setwd(file_loc)
  fns = list.files(recursive = T,
                   pattern = "\\.json$")
  fns2 = str_extract(fns, "[^_]+")
  inp = lapply(fns,
               fromJSON)
  l = as.numeric(length(map(inp,2)))
  s = seq(from = 1, to = l)
  f = NULL
  for (i in s) {
    if("content" %in% names(inp[[i]][[2]])){

      t = map(inp,2)[[i]] %>%
        select(timestamp_ms, sender_name, content) %>%
        mutate(sent_to = sub("/message", "", fns2[[i]]))
      if (i == 1){
        f=t
      } else {
        f = rbind(f, t)
      }
    }
  }
  setwd(wd)
  as.data.frame(f) %>%
    rename(sender = sender_name)
}
