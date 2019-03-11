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
#' @param from Which data source should be used. "canonical" for tips CSV, "twitter" for full Twitter data, "joined" for canonical joined on Twitter data and deduped
#' @param n_tweets Number of tweets to choose, or "all"
#' @param weighted Should tweets with higher scores be chosen more frequently? Will be false if from is "canonical"
#' @export
#' @importFrom dplyr sample_n
get_bunch_o_tweets <- function(from = "joined", n_tweets = "all", weighted = TRUE) {
  stopifnot(from %in% c("canonical", "twitter", "joined"))
  stopifnot(is.numeric(n_tweets) || n_tweets == "all")

  suppressMessages({
    if (from == "twitter") {
      tbl <- get_tweets(save_number = FALSE, n_tweets_to_grab = n_tweets) %>%
        score_tweets()
    } else if (from == "joined") {
      tbl <- readr::read_csv(joined_path)
    } else if (from == "canonical") {
      tbl <- readr::read_csv(tips_path)
    }
  })

  if (n_tweets == "all") {
    return(tbl)
  }

  if (from %in% c("joined", "twitter") && weighted) {
    tbl <- tbl %>%
      dplyr::sample_n(n_tweets, weight = score)
  } else if (from %in% c("joined", "twitter") && !weighted || from == "canonical") {
    tbl <- tbl %>%
      dplyr::sample_n(n_tweets)
  }

  tbl
}
