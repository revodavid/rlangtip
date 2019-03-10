## Script to convert Excel file of tips
## to ascii inst/extdata/tips.csv

library(readxl)
tips <- read_excel(here::here("data-raw", "Daily R Language Tips for RLangTip.xlsx"))

library(dplyr)
readr::write_csv(tips, here::here("inst", "extdata", "tips.csv"))

tips_2 <- readr::read_csv(here::here("inst", "extdata", "tips.csv"), col_types = "icc?c")

# verify roundtrip
all.equal(tips, tips_2) ## type mismatch for "id" is ok

#' Check duplicates

close_tips(tips, 0.20)

#' Generate spelling ignore wordlist

# get all packages on CRAN
pkgs <- available.packages()[, "Package"]

standard_packages <- unlist(tools:::.get_standard_package_names(), use.names = FALSE)
top_packages <- cranlogs::cran_top_downloads(count = 50)$package

# get function names for top 50 packages
library(purrr)
fun_names <- map(c(standard_packages, top_packages), possibly(getNamespaceExports, NULL)) %>% flatten_chr()

# Manually check the remaining words
spelling_errors <- as_tibble(spelling::spell_check_text(tip_text, ignore = c(pkgs, fun_names, wordlist))) %>%
  mutate(found = map_chr(found, glue::glue_collapse, ", "))

readr::write_csv(spelling_errors, "spelling.csv")

spelling_errors2 <- readr::read_csv("spelling2.csv")

to_ignore <- spelling_errors2$word[is.na(spelling_errors2$X3)]

wordlist <- c(pkgs, fun_names, to_ignore)

readr::write_lines(wordlist, here::here("inst", "extdata", "wordlist"))

#' Check spelling

check_tip_spelling(tips)
