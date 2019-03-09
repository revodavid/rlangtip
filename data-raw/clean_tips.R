library(readxl)
tips <- read_excel(here::here("Daily R Language Tips for RLangTip.xlsx"))

library(dplyr)

readr::write_csv(tips, here::here("inst", "extdata", "tips.csv"))

tips_2 <- readr::read_csv(here::here("inst", "extdata", "tips.csv"))

# verify roundtrip
all.equal(tips, tips_2)
