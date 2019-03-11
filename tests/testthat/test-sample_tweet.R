context("test-sample_tweet")

testthat::test_that("rtip produces string", {
  tip <- rtip(cowsay = FALSE)
  expect_is(tip, "character")
})


testthat::test_that("rtip produces expected string selected by id", {
  tip <- rtip(id = 1, cowsay = FALSE)
  expect_equal(
    tip,
    c(
      "Tip #1 in category statistics",
      "Use as.dendrogram() to prepare hierarchical clusters or trees for plotting #rstats http://bit.ly/1sshY0I",
      "      -- @RStudioJoe, 2010-06-09"
    )
  )
})

testthat::test_that("rtip produces an error if id doesn't exist", {
  tips <- readr::read_csv(system.file("extdata", "tips.csv", package = "rlangtip"), col_types = "icc?c")
  N <- NROW(tips)
  expect_error(rtip(id = (N + 1)))
})

testthat::test_that("rtip is expected length", {
  expect_length(rtip(), 3)
})
