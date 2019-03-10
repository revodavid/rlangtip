

#' Score tweets
#'
#' @param tbl 
#'
#' @return
#' @export
#'
#' @examples
score_tweets <- function(tbl) {
 tbl %>%
  mutate(
   score = favorite_count + 2 * retweet_count
  )
}

#' Join tips
#'
#' @param tweets 
#' @param fuzzy 
#'
#' @return
#' @export
#'
#' @examples
join_tips <- function(tweets, fuzzy = TRUE) {
 
 joiner_fun <- ifelse(fuzzy, fuzzyjoin::stringdist_left_join, dplyr::left_join)
 
 tips <- 
  read_csv(here("inst", "extdata", "tips.csv")) %>% 
  rename(text = Tip) 
 
 joined <- joiner_fun(tweets, tips, by = "text")
 
 joined %>% 
  dplyr::group_by(id) %>% 
  score_tweets() %>% 
  dplyr::add_tally(score) %>% 
  tidyr::drop_na(id) %>% 
  dplyr::distinct(id, .keep_all = TRUE)
}


# bar <- tweets %>% left_join(tips, by = "text")
# 
# baz <- bar %>% 
#  group_by(id) %>% 
#  score_tweets() %>% 
#  filter(is_canonical == TRUE) %>% 
#  distinct(id, .keep_all = TRUE)


bar %>% 
 select(id, text, favorite_count, retweet_count) %>% 
 dplyr::group_by(id) %>% 
 score_tweets() %>% 
 group_by(id) %>% 
 summarise(
  score = sum(score)
 )





