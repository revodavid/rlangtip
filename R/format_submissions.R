#' Add google sheet form data to csv file
#'
#' @param subs Tibble. Name of tibble containing submission data (output of get_submissions)
#' @param tips Tibble. Name of tips table (inst/extdata/tips.csv). Used to identify id ID range.
#'
#' @export
format_submissions <- function(subs, tips) {
  requireNamespace("dplyr")
  requireNamespace("readr")

  maxid <- max(tips$id, na.rm = TRUE)

  x <- subs %>%
    dplyr::mutate(
      id = seq(maxid + 1, maxid + nrow(subs)),
      Tip = `Suggested Tweet`,
      Author = `Your Name or Twitter Handle`,
      Last.Sent = "",
      Category = "Uncategorized"
    ) %>%
  dplyr::select(id, Tip, Author, Last.Sent, Category)

  colnames(x) <- NULL

  readr::write_csv(x, file("clipboard"))
}

utils::globalVariables(c("Suggested Tweet", "Your Name or Twitter Handle", "id", "Tip", "Author", "Last.Sent", "Category"))
