#' Return a table of close tips
#'
#' @param text data.frame of tips
#' @param cutoff the Z score cutoff to use
#' @inheritParams stringdist::stringdistmatrix
#' @param ... Additional parameters passed to [stringdist::stringdistmatrix]
#' @export
similar_text <- function(text, cutoff = .15, method = "jw", ...) {
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
#' @importFrom dplyr as_tibble rename filter mutate distinct select
filter_dupes <- function(tbl, cutoff = -3) {
  dists <-
    expand.grid(tbl$text, tbl$text) %>%
    as_tibble() %>%
    rename(
      tweet_1 = Var1,
      tweet_2 = Var2
    ) %>%
    filter(tweet_1 != tweet_2) %>%
    mutate(
      string_dist = stringdist::stringdist(tweet_1, tweet_2),
      string_dist_scaled = z_score(string_dist)
    ) %>%
    filter(
      string_dist_scaled > cutoff
    ) %>%
    distinct(tweet_1) %>%
    select(tweet_1) %>%
    rename(
      text = tweet_1
    )
}
utils::globalVariables(c(
  "Var1", "Var2", "tweet_1", "tweet_2", "string_dist",
  "string_dist_scaled"
))
