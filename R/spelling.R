#' Check spelling of tips
#'
#' @param tips data.frame of tips
#' @inheritParams spelling::spell_check_text
#' @export
check_tip_spelling <- function(tips, ignore = readLines(system.file("extdata", "wordlist", package = "rlangtip"))) {
  tip_text <- tips$Tip

  # Remove links
  tip_text <- gsub("https?://\\S+", "", tip_text)
  tip_text <- gsub("bit.ly\\S+", "", tip_text)

  tibble::as_tibble(spelling::spell_check_text(tip_text, ignore = ignore))
}
