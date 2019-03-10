

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
  # rename(text = Tip) %>% 
  mutate(is_canonical = TRUE)
 
 last_run_day <- readr::read_lines(here::here("last_run_day.txt")) %>% 
  as.Date()
 
 readr::write_lines(Sys.Date(), here::here("last_run_day.txt"))
 
 max_tips_id <- max(tips$id, na.rm = TRUE)
 
 tweets <- 
  tweets %>% 
  mutate(
   is_canonical = 
    case_when(created_at > last_run_day ~ TRUE,
              TRUE ~ FALSE),
   id = 
    case_when(
     is_canonical ~ max_tips_id + 1 : nrow(.),
     TRUE ~ NA_real_
    )
  ) 
 
 joined <- joiner_fun(tweets, tips, by = c("text" = "Tip")) %>% 
  select(id.x, id.y, favorite_count, retweet_count) %>% 
  mutate(
   id = 
    case_when(is.na(id.x) ~ id.y,
              is.na(id.y) ~ id.x,
              TRUE ~ NA_real_)
  ) %>% 
  select(-id.x, -id.y)
 
 scores <- joined %>% 
  dplyr::group_by(id) %>% 
  score_tweets() %>% 
  group_by(id) %>% 
  summarise(
   score = sum(score)
  )
 
 joined %>% 
  tidyr::drop_na(id) %>% 
  dplyr::distinct(id, .keep_all = TRUE) %>% 
  left_join(scores)
 
}








# bar <- tweets %>% left_join(tips, by = "text")
# 
# baz <- bar %>% 
#  group_by(id) %>% 
#  score_tweets() %>% 
#  filter(is_canonical == TRUE) %>% 
#  distinct(id, .keep_all = TRUE)


scores <- bar %>% 
 select(id, text, favorite_count, retweet_count) %>% 
 dplyr::group_by(id) %>% 
 score_tweets() %>% 
 group_by(id) %>% 
 summarise(
  score = sum(score)
 )

bar %>% 
 left_join(tips)




