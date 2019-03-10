

#' Get tweet number
#'
#' @param buffer Numeric. The number to add to the last number of tweets pulled in such that we always ask for more tweets than necessary the next time the script is run.
#'
#' @return
#' @export
#'
#' @examples
#'
get_tweet_number <- function(buffer = 100) {
  last_val <-
    readr::read_lines(n_tweets_path) %>%
    as.numeric()

  last_val + buffer
}

#' Save tweet number
#'
#' @param val
#'
#' @return
#' @export
#'
#' @examples
#'

save_tweet_number <- function(val) {
  readr::write_lines(val, n_tweets_path)
}

#' Get Tweets
#'
#' @param save_number
#'
#' @return
#' @export
#'
#' @examples
get_tweets <- function(save_number = TRUE) {
  n_tweets_to_grab <-
    get_tweet_number()

  tbl <- rtweet::get_timeline("RLangTip", n = n_tweets_to_grab)

  if (save_number) {
    n_tweets <- nrow(tbl)
    save_tweet_number(n_tweets)
  }

  tbl %>%
    select(status_id, created_at, text, favorite_count, retweet_count)
}


#' Add #rstats
#'
#' @param txt Tip text.
#' @param col Column to add hashtags to.
#'
#' @return
#' @export
#'
#' @examples
#'
#' tibble(text = "foo") %>% add_rstats_hashtag()
#' tibble(text = "foo #rstats") %>% add_rstats_hashtag
#'
add_rstats_hashtag <- function(tbl, col = text) {
  q_col <- enquo(col)

  tbl %>%
    mutate(
      text =
        case_when(
          !str_detect(!!q_col, "#rstats") ~ !!q_col %>% str_c(" #rstats"),
          TRUE ~ text
        )
    )
}
