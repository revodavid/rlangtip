## good tweets to test
## 366

## TODO
## Don't import file each time -- use an object created on load
## Allow printing of a specific tweet by id
## Don't show random uncategorized or deprecated tweets
## Allow filtering by category
## Word wrap tweet to width of display

rtip <- function(id) {
 ## Print a random tweet from tips.csv 
 tips <- readr::read_csv(here::here("inst", "extdata", "tips.csv"),col_types="icc?c")
 N <- NROW(tips)
 if(missing(id)) rownum <- sample(1:N,1) else {
  rownum <- which(tips$id == id)
  if(length(rownum)==0) stop("No such tip")
 }
 tiprow <- tips[rownum,]
 tiprow
 display <- c(paste0("Tip #", tiprow$id, " in category ",tiprow$Category, sep=""),
              tiprow$Tip,
              paste0("      -- ", tiprow$Author, ", ", tiprow$"Last Sent"))
 cat(display,sep="\n")
 invisible(display)
}
