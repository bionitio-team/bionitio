context("pretty")

test_that("pretty_output works with non-zero sequences", {
  input <- list(filename = NULL, numseq = 10, total = 1000, min = 10,
                avg = 100, max = 1000)
  expect <- input
  expect_equal(pretty_output(input), expect)
})

test_that("pretty_output works with zero sequences", {
  input <- get_seq_stats(NULL)
  expect <- list(filename = NULL, numseq = 0, total = 0, min = "-",
                 avg = "-", max = "-")
  expect_equal(pretty_output(input), expect)
})
