

score_tweets <- function(tbl) {
 tbl %>% 
  mutate(
   score = favorite_count + 2*retweet_count
  )
}


tips <- read_csv(here::here("data-raw", "tips.csv"))
