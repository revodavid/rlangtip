
testthat::test_that("rtip produces string", {
  tip <- rtip()
  expect_is(tip, "character")
})


testthat::test_that("rtip produces expected string selected by id", {
  tip <- rtip(id = 1)
  expect_equal(tip, cat(
    "Tip #1 in category Uncategorized
Use as.dendrogram() to prepare hierarchical clusters or trees for plotting #rstats http://bit.ly/1sshY0I
      -- @RStudioJoe, 2010-06-09",
    sep = "\n"
  ))
})

testthat::test_that("rtip produces an error if id doesn't exist", {
  tips <- readr::read_csv(here::here("inst", "extdata", "tips.csv"), col_types = "icc?c")
  N <- NROW(tips)
  expect_error(rtip(id = (N + 1)))
})

testthat::test_that("rtip is expected length", {
 expect_length(rtip(), 1)
})
