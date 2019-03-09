

save_tweet_number <- function(val) {
 readr::write_lines(val)
}

get_rlangtip_tweets <- function(n = 100, save_number = TRUE) {
 tbl <- rtweet::get_timeline("RLangTip", n = n)
 
 if (save_number) {
  n_tweets <- nrow(tbl)
  save_tweet_number(n_tweets)
 }
 
 tbl %>% 
  select(status_id, created_at, text, favorite_count, retweet_count)
}


score_rlangtip_tweets <- function(tbl) {
 
 tbl %>% 
  mutate(
   score = favorite_count + 2*retweet_count
  )
}

get_tweet_number <- function(buffer = 100) {
 last_val <- 
  readr::read_lines(here::here("n_tweets.txt")) %>% 
  as.numeric()
 
 last_val + buffer
}




