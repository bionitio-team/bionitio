context("stats")

test_that("get_seq_stats works for valid sequences", {
  e1 <- list(filename = NULL, numseq = 1, total = 1, min = 1, avg = 1, max = 1)
  expect_equal(get_seq_stats("A"), e1)

  e2 <- list(filename = NULL, numseq = 1, total = 4, min = 4, avg = 4, max = 4)
  expect_equal(get_seq_stats("ACGT"), e2)

  e3 <- list(filename = NULL, numseq = 3, total = 24, min = 4, avg = 8,
             max = 12)
  expect_equal(get_seq_stats(c("ACGT", "AAAAAAAA", "CCCCCCCCCCCC")), e3)
})
