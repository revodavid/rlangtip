#' Return a table of close tips
#'
#' @param tips data.frame of tips
#' @param cutoff the cutoff to use
#' @inheritParams stringdist::stringdistmatrix
#' @param ... Additional parameters passed to [stringdist::stringdistmatrix]
#' @export
close_tips <- function(tips, cutoff = 0.15, method = "jw", ...) {
  diffs <- stringdist::stringdistmatrix(tips$Tip, tips$Tip, method = method)
  diffs[lower.tri(diffs, diag = TRUE)] <- NA
  close <- which(diffs < cutoff)

  tibble::tibble(
    tip_1_id = col(diffs)[close],
    tip_2_id = row(diffs)[close],
    score = diffs[close],
    tip_1 = tips$Tip[tip_1_id],
    tip_2 = tips$Tip[tip_2_id]
  )
}



#' Filter Dupes
#'
#' @param tbl 
#' @param cutoff 
#'
#' @return
#' @export
#'
#' @examples
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
      string_dist_scaled = dobtools::z_score(string_dist)
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