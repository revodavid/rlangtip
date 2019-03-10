n_tweets_path <- here::here("n_tweets.txt")
tips_path <- here::here("tips.csv")

utils::globalVariables(c("consumer_key", "consumer_secret", "access_token", "access_secret"))
save_twitter_token <- function() {
 source(here::here("twitter_keys.R"))

 rtweet::create_token(
  app = "RLangTip",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)
}
