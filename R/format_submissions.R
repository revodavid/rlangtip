#' Add google sheet form data to csv file
#'
#' @param subs Tibble. Name of tibble containing submission data (output of get_submissions)
#' @param tips Tibble. Name of tips table (inst/extdata/tips.csv). Used to identify id ID range.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 

format_submissions <- function(subs, tips) {
 
 maxid <- max(tips$id, na.rm = T)

 x <- subs %>%
  mutate(id = seq(maxid+1, maxid+nrow(subs)),
         Tip = `Suggested Tweet`, 
         Author = `Your Name or Twitter Handle`, 
         Last.Sent = "",
         Category = "Uncategorized") %>% 
  select(id, Tip, Author, Last.Sent, Category) 
 
 colnames(x) <- NULL
 
 write.csv(x, file("clipboard"), row.names=F)
 #write.csv(x, here::here("inst", "extdata", "NewTips.csv"), row.names=F, con)
 #cat(scan(here::here("inst", "extdata", "NewTips.csv"), character(), sep="\n"), sep="\n")

}



