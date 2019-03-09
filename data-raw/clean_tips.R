library(readxl)
raw_tips <- read_excel(here::here("Daily R Language Tips for RLangTip.xlsx"))

library(dplyr)

tips <- raw_tips %>% select(-X__1)
readr::write_csv(tips, here::here("data-raw", "tips.csv"))
