#' Score tweets
#'
#' @param tbl the tibble to import
#'
#' @export
score_tweets <- function(tbl) {
  requireNamespace("dplyr")

  tbl %>%
    dplyr::mutate(
      score = favorite_count + 2 * retweet_count
    )
}
utils::globalVariables(c("favorite_count", "retweet_count"))

#' Clean tweets
#'
#' @param x Tweet tweets to include
#'
#' @export
clean_tweets <- function(x) {
  requireNamespace("stringr")
  requireNamespace("textclean")
  x %>%
    stringr::str_remove_all("https?://\\S+") %>%
    stringr::str_remove_all("#rstats") %>%
    textclean::replace_html() %>%
    stringr::str_trim()
}

#' Join tips
#'
#' @param tweets tweet dataset
#' @param fuzzy join fuzzily
#' @export
join_tips <- function(tweets, fuzzy = TRUE) {
  requireNamespace("here")
  requireNamespace("fuzzyjoin")
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  requireNamespace("readr")

  joiner_fun <- ifelse(fuzzy, fuzzyjoin::stringdist_left_join, dplyr::left_join)

  tips <-
    readr::read_csv(system.file("extdata/tips.csv", package = "rlangtip")) %>%
    # rename(text = Tip) %>%
    dplyr::mutate(is_canonical = TRUE) %>%
    dplyr::mutate(
      Tip = clean_tweets(Tip)
    )

  last_run_day <- readr::read_lines(here::here("last_run_day.txt")) %>%
    as.Date()

  readr::write_lines(Sys.Date(), here::here("last_run_day.txt"))

  max_tips_id <- max(tips$id, na.rm = TRUE)

  tweets <-
    tweets %>%
    dplyr::mutate(
      is_canonical =
        dplyr::case_when(
          created_at > last_run_day ~ TRUE,
          TRUE ~ FALSE
        ),
      id =
        dplyr::case_when(
          is_canonical ~ as.numeric(1:nrow(.)),
          TRUE ~ NA_real_
        )
    ) %>%
    dplyr::mutate(
      text = clean_tweets(text)
    )

  joined <- joiner_fun(tweets, tips, by = c("text" = "Tip")) %>%
    dplyr::select(id.x, id.y, favorite_count, retweet_count) %>%
    dplyr::mutate(
      id =
        dplyr::case_when(
          is.na(id.x) ~ id.y,
          is.na(id.y) ~ id.x,
          TRUE ~ NA_real_
        )
    ) %>%
    dplyr::select(-id.x, -id.y)

  scores <- joined %>%
    dplyr::group_by(id) %>%
    score_tweets() %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      score = sum(score)
    )

  joined %>%
    tidyr::drop_na(id) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::left_join(scores)
}

utils::globalVariables(c("id.x", "id.y", "score"))
