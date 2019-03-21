
weight_determination <- function(weighted = F) {
  if (weighted == F) {
    return(NULL)
  } else {
    weighted <- score()
  }
}

sample_tweet <- function(number_of_tweets = 1, holiday = FALSE, categories = NULL, weighted = FALSE) {
  requireNamespace("readxl")

  tweets <- readxl::read_excel("data_cleaned.xlsx")

  if (length(categories) != 0) {
    tweets <- tweets[(tweets$category %in% categories), ]
  }

  if (!holiday) {
    tweets <- tweets[tweets$category != "holiday", ]
  }

  if (weighted) {
    n <- dplyr::sample_n(tweets, number_of_tweets, weight = score)
  } else {
    n <- dplyr::sample_n(tweets, number_of_tweets, weight = NULL)
  }

  return(n)
}
