
utils::globalVariables(c("favorite_count", "retweet_count"))

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

#' Clean tweets
#'
#' @param x Tweet
#'
#' @return
#' @export
#'
#' @examples
clean_tweets <- function(x) {
 x %>% 
  stringr::str_remove_all("https?://\\S+") %>% 
  stringr::str_remove_all("#rstats") %>% 
  textclean::replace_html() %>% 
  stringr::str_trim()
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
  mutate(is_canonical = TRUE) %>% 
  mutate(
   Tip = clean_tweets(Tip)
  )
 
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
     is_canonical ~ as.numeric(1: nrow(.)), 
     TRUE ~ NA_real_
    )
  ) %>% 
  mutate(
   text = clean_tweets(text)
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

