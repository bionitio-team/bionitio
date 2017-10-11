context("run_bionitio")

test_that("run_bionitio returns expected output", {
  my_fasta_files <- c(
    system.file("extdata", "one_sequence.fasta", package = "bionitio"),
    system.file("extdata", "two_sequence.fasta", package = "bionitio"))
  expect <- matrix(
    data = c(my_fasta_files, 1, 2, 237, 357, 237, 120, 237, 178, 237, 237),
    nrow = 2,
    dimnames = list(NULL, c("filename", "numseq", "total", "min", "avg", "max"))
  )

  expect_equal(run_bionitio(my_fasta_files), expect)
})
