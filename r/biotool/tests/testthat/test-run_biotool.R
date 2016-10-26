context("run_biotool")

test_that("run_biotool returns expected output", {
  my_fasta_files <- c(
    system.file("extdata", "one_sequence.fasta", package = "biotool"),
    system.file("extdata", "two_sequence.fasta", package = "biotool"))
  expect <- matrix(
    data = c(my_fasta_files, 1, 2, 237, 357, 237, 120, 237, 178, 237, 237),
    nrow = 2,
    dimnames = list(NULL, c("filename", "numseq", "total", "min", "avg", "max"))
  )

  expect_equal(run_biotool(my_fasta_files), expect)
})
