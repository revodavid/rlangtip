

save_tweets <- function(tbl) {
 # Will be whatever format Jim decides
 write_csv(tbl, tips_path)
}


run_tweet_pipeline <- function() {
 get_tweets() %>% 
  score_tweets() %>% 
  save_tweets()
}