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
   join_tips() %>%
   save_tweets()
}


#' Get a bunch of tweets
#'
#' @param n_tweets Number of tweets to choose
#' @param weighted Should tweets with higher scores be chosen more frequently?
#'
#' @return
#' @export
#'
#' @examples
get_bunch_o_tweets <- function(n_tweets = 5, weighted = TRUE) {
 joined <- readr::read_csv(here::here("data-raw", "joined.csv"))
 
 joined %>% 
  sample_n(n_tweets, weight = score)
} 
