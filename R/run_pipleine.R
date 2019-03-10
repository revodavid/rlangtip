#' Save Tweets
#'
#' @param tbl a tibble of tweets
#'
#' @export
save_tweets <- function(tbl) {
  # Will be whatever format Jim decides
  readr::write_csv(tbl, tips_path)
}


#' Get, score, and save tweets
#'
#' @export
run_tweet_pipeline <- function() {
  get_tweets() %>%
    score_tweets() %>%
    add_rstats_hashtag() %>%
    save_tweets()
}
