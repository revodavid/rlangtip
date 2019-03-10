## good tweets to test
## 366

## TODO
## Don't import file each time -- use an object created on load
## Don't show random uncategorized or deprecated tweets
## Allow filtering by category
## Word wrap tweet to width of display

#' Return the tips database
#'
#' @return Table of tips in tibble format
read_tips <- function() {
 readr::read_csv(here::here("inst", "extdata", "tips.csv"), col_types = "icc?c")
}

#' Display an R tip
#'
#' @param id numeric ID of tip to display. If not provided, display a randomly-selected tip.
#' @param cowsay if TRUE, a random cowsay animal will present the tip
#' @param color if TRUE, use a colorful cowsay display. Ignored if cowsay=FALSE
#'
#' @return
#' @export
rtip <- function(id, cowsay = TRUE, color = FALSE) {
  ## Print a random tweet from tips.csv
  tips <- read_tips()
  N <- NROW(tips)
  if (missing(id)) {
    rownum <- sample(1:N, 1)
  } else {
    rownum <- which(tips$id == id)
    if (length(rownum) == 0) stop("No such tip")
  }
  tiprow <- tips[rownum, ]
  tiprow

  if (cowsay) {
    on_windows <- function() {
      os <- tolower(Sys.info()[["sysname"]])

      "windows" %in% os
    }

    who_pool <- names(cowsay::animals)

    if (on_windows()) {
      no_windows <- c("shortcat", "longcat", "fish", "signbunny", "stretchycat", "anxiouscat", "longtailcat", "grumpycat", "mushroom")
      who_pool <- names(cowsay::animals)[-which(names(cowsay::animals) %in% no_windows)]
    }

    who <- sample(who_pool, 1)

    display <- c(
      paste0("Tip #", tiprow$id, " in category ", tiprow$Category, sep = ""),
      tiprow$Tip,
      paste0("      -- ", tiprow$Author, ", ", tiprow$"Last Sent")
    )

    display_cat <- display %>% paste(collapse = "\n")

    if(color) { 
    cowsay::say(display_cat, by_color = "rainbow", by = who, type = "string") %>%
      cat()
    } else {
     cowsay::say(display_cat, by = who, type = "string") %>%
      cat()
     
     return(invisible(display_cat))
    }
  } else {
    display <- c(
      paste0("Tip #", tiprow$id, " in category ", tiprow$Category, sep = ""),
      tiprow$Tip,
      paste0("      -- ", tiprow$Author, ", ", tiprow$"Last Sent")
    )

    cat(display, sep = "\n")
    
    return(invisible(display_cat))
  } 
}
