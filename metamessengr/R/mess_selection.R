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
#' @importFrom dplyr select mutate rename
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom stringr str_extract
#' @importFrom magrittr %>%
#'
#' @export
mess_selection <- function(file_loc) {
  if(base::is.null(file_loc)){
    file_loc = getwd()
  }
  wd <- base::getwd()
  base::setwd(file_loc)
  fns = base::list.files(recursive = T,
                   pattern = "\\.json$")
  fns2 = stringr::str_extract(fns, "[^_]+")
  inp = base::lapply(fns,
               jsonlite::fromJSON)
  l = base::as.numeric(length(purrr::map(inp,2)))
  s = base::seq(from = 1, to = l)
  f = NULL
  for (i in s) {
    if("content" %in% base::names(inp[[i]][[2]])){

      t = purrr::map(inp,2)[[i]] %>%
        dplyr::select(timestamp_ms, sender_name, content) %>%
        dplyr::mutate(sent_to = base::sub("/message", "", fns2[[i]]))
      if (i == 1){
        f=t
      } else {
        f = base::rbind(f, t)
      }
    }
  }
  base::setwd(wd)
  base::as.data.frame(f) %>%
    dplyr::rename(sender = sender_name)
}
