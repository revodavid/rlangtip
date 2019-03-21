#' Return a table of close tips
#'
#' @param text data.frame of tips
#' @param cutoff the Z score cutoff to use
#' @param ... Additional parameters passed to [stringdist::stringdistmatrix]
#' @param method Method for distance calculation, the default is "osa". See
#'   [stringdist::`stringdist-encoding`] for details.
#' @export
similar_text <- function(text, cutoff = .15, method = "jw", ...) {
  requireNamespace("stringdist")
  requireNamespace("tibble")

  diffs <- stringdist::stringdistmatrix(text, text, method = method)
  diffs[lower.tri(diffs, diag = TRUE)] <- NA
  close <- which(diffs < cutoff)

  tibble::tibble(
    tip_1_id = col(diffs)[close],
    tip_2_id = row(diffs)[close],
    score = diffs[close],
    tip_1 = text[tip_1_id],
    tip_2 = text[tip_2_id]
  )
}
utils::globalVariables(c("tip_1_id", "tip_2_id"))

#' Filter Dupes
#'
#' @param tbl A tibble
#' @param cutoff The Z-score cutoff to use for filtering
#'
#' @export
filter_dupes <- function(tbl, cutoff = -3) {
  requireNamespace("stringdist")
  requireNamespace("tibble")
  requireNamespace("dplyr")

  dists <-
    expand.grid(tbl$text, tbl$text) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      tweet_1 = Var1,
      tweet_2 = Var2
    ) %>%
    dplyr::filter(tweet_1 != tweet_2) %>%
    dplyr::mutate(
      string_dist = stringdist::stringdist(tweet_1, tweet_2),
      string_dist_scaled = z_score(string_dist)
    ) %>%
    dplyr::filter(
      string_dist_scaled > cutoff
    ) %>%
    dplyr::distinct(tweet_1) %>%
    dplyr::select(tweet_1) %>%
    dplyr::rename(
      text = tweet_1
    )
}
utils::globalVariables(c(
  "Var1", "Var2", "tweet_1", "tweet_2", "string_dist",
  "string_dist_scaled"
))
