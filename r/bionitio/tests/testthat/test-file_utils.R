context("file_utils")

test_that("fasta_exists returns TRUE when files exist", {
  my_fasta_files <- c(
    system.file("extdata", "one_sequence.fasta", package = "bionitio"),
    system.file("extdata", "two_sequence.fasta", package = "bionitio"))

  expect_true(fasta_exists(my_fasta_files))
})

test_that("fasta_exists fails with non-existant file", {
  filename <- "non_existant.fasta"
  expect_error(fasta_exists(filename))
})

test_that("fasta_permission returns TRUE when files have read permission", {
  my_fasta_files <- c(
    system.file("extdata", "one_sequence.fasta", package = "bionitio"),
    system.file("extdata", "two_sequence.fasta", package = "bionitio"))

  expect_true(fasta_permission(my_fasta_files))
})
