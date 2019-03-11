n_tweets_path <- function() {
  system.file("extdata", "n_tweets.txt", package = "rlangtip")
}

tips_path <- function() {
  system.file("extdata", "tips.csv", package = "rlangtip")
}

joined_path <- function() {
  system.file("extdata", "joined.csv", package = "rlangtip")
}

get_wordlist <- function() {
  readLines(system.file("extdata", "wordlist", package = "rlangtip"))
}

utils::globalVariables(c("consumer_key", "consumer_secret", "access_token", "access_secret"))
save_twitter_token <- function() {
  requireNamespace("rtweet")
  source(here::here("twitter_keys.R"))

  rtweet::create_token(
    app = "RLangTip",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret
  )
}
