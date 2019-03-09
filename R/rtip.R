## good tweets to test
## 366

## TODO
## Allow printing of a specific tweet by id
## Don't show random uncategorized tweets
## Allow filtering by category

rtip <- function() {
 ## Print a random tweet from tips.csv 
 tips <- readr::read_csv(here::here("inst", "extdata", "tips.csv"))
 tiprow <- tips[sample(1:NROW(tips),1),]
 tiprow
 display <- c(paste0("Tip #", tiprow$id, " in category ",tiprow$Category, sep=""),
              tiprow$Tip,
              paste0("      -- ", tiprow$Author, ", ", tiprow$"Last Sent"))
 cat(display,sep="\n")
 invisible(display)
}
