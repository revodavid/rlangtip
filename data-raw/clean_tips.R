library(readxl)
tips <- read_excel(here::here("Daily R Language Tips for RLangTip.xlsx"))

library(dplyr)

readr::write_csv(tips, here::here("inst", "extdata", "tips.csv"))

tips_2 <- readr::read_csv(here::here("inst", "extdata", "tips.csv"),col_types="icc?c")

# verify roundtrip
all.equal(tips, tips_2) ## type mismatch for "id" is ok
