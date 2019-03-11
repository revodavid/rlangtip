

#' Get tweet number
#'
#' @param buffer Numeric. The number to add to the last number of tweets pulled in such that we always ask for more tweets than necessary the next time the script is run.
#'
#' @export
#' @importFrom dplyr %>%
#'
get_tweet_number <- function(buffer = 100) {
  last_val <-
    readr::read_lines(n_tweets_path) %>%
    as.numeric()

  last_val + buffer
}

#' Save tweet number
#'
#' @param val the vector of tweet numbers
#'
#' @export
#'
save_tweet_number <- function(val) {
  readr::write_lines(val, n_tweets_path)
}

#' Get Tweets
#'
#' @param n_tweets_to_grab Number of tweets to grab, or "all"
#' @param save_number Whether to save the numbers of the tweets or not
#' @export
get_tweets <- function(save_number = TRUE, n_tweets_to_grab = "all") {
  if (n_tweets_to_grab == "all") {
    n_tweets_to_grab <-
      get_tweet_number()
  }

  tbl <- rtweet::get_timeline("RLangTip", n = n_tweets_to_grab)

  if (save_number && n_tweets_to_grab == "all") {
    n_tweets <- nrow(tbl)
    save_tweet_number(n_tweets)
  }

  tbl %>%
    select(status_id, created_at, text, favorite_count, retweet_count)
}
utils::globalVariables(c("status_id", "created_at", "text"))


#' Add #rstats
#'
#' @param tbl Tip text.
#' @param col Column to add hashtags to.
#'
#' @export
#'
#' @importFrom dplyr enquo case_when
#' @examples
#'
#' tibble::tibble(text = "foo") %>% add_rstats_hashtag(text)
#' tibble::tibble(text = "foo #rstats") %>% add_rstats_hashtag(text)
#'
add_rstats_hashtag <- function(tbl, col) {
  q_col <- enquo(col)

  tbl %>%
    mutate(
      text =
        case_when(
          !stringr::str_detect(!!q_col, "#rstats") ~ !!q_col %>% stringr::str_c(" #rstats"),
          TRUE ~ text
        )
    )
}
