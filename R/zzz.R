
n_tweets_path <- here::here("n_tweets.txt")

save_token <- function() {
 source(here::here(twitter_keys.R))
 
 create_token(
  app = "RLangTip",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  acess_token = acess_token,
  access_secret = access_secret)
}