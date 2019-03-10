#' Import Google Sheets Data
#'
#' Run googlesheets:gs_ls() to initiate authentication
#'
#' @param key Character. The key value associated with the google sheet which has the form responses.
#' @param tab Character. Name of the google sheet tab with the form responses.
#'
#'
#' @export
get_submissions <- function(key, tab) {
  rLangSheet <- googlesheets::gs_key(key)
  rLangSheet %>%
    googlesheets::gs_read(ws = tab)
}
