
get_rlangtip_tweets <- function(n = 100) {
 tbl <- rtweet::get_timeline("RLangTip", n = n)
 
 tbl <- tbl %>% 
  select(status_id, created_at, text, favorite_count, retweet_count)
}


score_rlangtip_tweets <- function(tbl) {
 
 tbl %>% 
  mutate(
   score = favorite_count + 2*retweet_count
  )
}
