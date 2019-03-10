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

 subs %>%
  mutate(id = seq(maxid+1, maxid+nrow(subtest)),
         Tip = `Suggested Tweet`, 
         Author = `Your Name or Twitter Handle`, 
         Last.Sent = NA,
         Category = "Uncategorized") %>% 
  select(id, Tip, Author, Last.Sent, Category) 

}



