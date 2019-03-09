

score_tweets <- function(tbl) {
 tbl %>% 
  mutate(
   score = favorite_count + 2*retweet_count
  )
}
