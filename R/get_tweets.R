

get_tweet_number <- function(buffer = 100) {
 last_val <- 
  readr::read_lines(n_tweets_path) %>% 
  as.numeric()
 
 last_val + buffer
}

save_tweet_number <- function(val) {
 readr::write_lines(val, n_tweets_path)
}

get_rlangtip_tweets <- function(save_number = TRUE) {
 n_tweets_to_grab <- 
  get_tweet_number()
 
 tbl <- rtweet::get_timeline("RLangTip")
 
 if (save_number) {
  n_tweets <- nrow(tbl)
  save_tweet_number(n_tweets)
 }
 
 tbl %>% 
  select(status_id, created_at, text, favorite_count, retweet_count)
}



