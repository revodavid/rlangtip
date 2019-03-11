## good tweets to test
## 366 573 506

## TODO
## Don't import file each time -- use an object created on load
## Don't show random uncategorized or deprecated tweets
## Allow filtering by category
## Word wrap tweet to width of display

#' Return the tips database
#'
#' @return Table of tips in tibble format
read_tips <- function() {
  readr::read_csv(system.file("extdata/tips.csv", package = "rlangtip"), col_types = "icc?c")
}

#' Display an R tip
#'
#' @param id numeric ID of tip to display. If not provided, display a randomly-selected tip.
#' @param cowsay if TRUE, a random cowsay animal will present the tip
#' @param color if TRUE, use a colorful cowsay display. Ignored if cowsay=FALSE
#' @param excluded categories excluded for random tip selection
#' @param keyword character vector keywords contained in tips to search for.
#'
#' @return the tip, as a 3-element vector (tip number and category; tip text; author and date)
#' @export
rtip <- function(id, cowsay = TRUE, color = FALSE,
                 excluded = c("deprecated", "Uncategorized"),
                 keyword = NULL) {
  ## Print a random tweet from tips.csv
  tips <- read_tips()

  if (!is.null(keyword)) {
    if (!is.character(keyword)) stop("keyword must be of class character.")

    keyword <- keyword %>%
      unique() %>%
      tolower()

    keyword <- stringr::str_c(keyword, collapse = "|")

    tips <- tips %>%
      dplyr::filter(str_detect(tolower(Tip), keyword))
  }

  N <- NROW(tips)
  if (missing(id)) {
    candidates <- (1:N)[!(tips$Category %in% excluded)]
    rownum <- sample(candidates, 1)
  } else {
    rownum <- which(tips$id == id)
    if (length(rownum) == 0) stop("No such tip")
  }
  tiprow <- tips[rownum, ]
  tiprow

  ### Word-wrap the tip to fit the terminal
  wrappedTip <- strwrap(tiprow$Tip)

  display <- c(
    paste0("Tip #", tiprow$id, " in category ", tiprow$Category, sep = ""),
    wrappedTip,
    paste0("      -- ", tiprow$Author, ", ", tiprow$"Last Sent")
  )

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

    display <- display %>% paste(collapse = "\n")

    if (color) {
      cowsay::say(display, by_color = "rainbow", by = who, type = "string") %>%
        cat()
    } else {
      cowsay::say(display, by = who, type = "string") %>%
        cat()
    }
  } else {
    cat(display, sep = "\n")
  }

  invisible(display)
}
