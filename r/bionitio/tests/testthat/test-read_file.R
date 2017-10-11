context("read_file")

test_that("load_fasta_file can read in valid fasta files", {
  filename <- system.file("extdata", "two_sequence.fasta", package = "bionitio")
  seq <- load_fasta_file(filename)

  expect_length(seq, 2)
  expect_equal(nchar(seq[1]), 237)
  expect_equal(nchar(seq[2]), 120)
  expect_match(seq[1],
               "^MTEITAAMVKELRESTGAGMMDCKNALSETNGDFDKAVQLLREKGLGKAAKKADRLAAEG")
  expect_match(seq[2],
               "ATIGENLVVRRFATLKAGANGVVNGYIHTNGRVGVVIAAACDSAEVASKSRDLLRQICMH$")
  expect_equal(class(seq), "character")
})

test_that("load_fasta_file returns NULL for empty files", {
  filename <- system.file("extdata", "empty_file", package = "bionitio")
  expect_null(load_fasta_file(filename))
})

test_that("load_fasta_file errors on invalid fasta files", {
  expect_error(load_fasta_file("single_greather_than.fasta",
                               quit_on_error = FALSE))
})
