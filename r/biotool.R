#!/usr/bin/env Rscript

VERSION <- "1.0"
DEFAULT_MIN_LEN <- 0
DEFAULT_VERBOSE <- FALSE

suppressPackageStartupMessages({
  library(argparse, quietly = TRUE)
  library(seqinr, quietly = TRUE)
})

parser <- ArgumentParser(description = "Print FASTA stats")
parser$add_argument("fasta_files",
                    metavar = "FASTA_FILE",
                    type = "character",
                    nargs = "+",
                    help = "Input FASTA files. Use - to read from stdin")
parser$add_argument("--minlen",
                    metavar = "N",
                    type = "integer",
                    dest = "min_len",
                    default = DEFAULT_MIN_LEN,
                    help = paste0("Minimum length sequence to include in stats",
                                  " [default: %(default)s]"))
parser$add_argument("--verbose",
                    dest = "verbose",
                    action = "store_true",
                    default = DEFAULT_VERBOSE,
                    help = "Print more stuff about what's happening")
parser$add_argument("--version",
                    dest = "print_version",
                    action = "store_true",
                    help = "Print version and exit")

# Print version
if ("--version" %in% commandArgs()) {
  cat(basename(commandArgs()[4]), VERSION, "\n")
  quit(save = "no")
}

# Process command line arguments
args <- parser$parse_args()

# Read from stdin if file is '-'
args$fasta_files[args$fasta_files == '-'] <- 'stdin'

# Get statistics of a FASTA file
get_fasta_stats <- function(filename, min_len) {
  # Calculate statistics of a FASTA file.
  #
  # Args:
  #   filename: The name of the input FASTA file.
  #   min_len: The minimum length of the sequence to include when calculating
  #            statistics.
  # Returns:
  #   A list containing FASTA stats.
  min_seq <- Inf
  max_seq <- 0
  num_seq <- 0
  num_bases <- 0

  sequences <- tryCatch(
    read.fasta(file = filename, seqtype = "AA", seqonly = TRUE),
    error = function(e) {
      if (args$verbose) warning(filename, " has no sequences.", call. = FALSE)
      return(NULL)
    }
  )
  for (seq in sequences) {
    this_len <- nchar(seq[1])
    if (this_len >= min_len) {
      num_seq <- num_seq + 1
      min_seq <- min(min_seq, this_len)
      max_seq <- max(max_seq, this_len)
      num_bases <- num_bases + this_len
    }
  }
  min_seq <- ifelse(num_seq == 0, 0, min_seq)
  return(list(filename = filename, numseq = num_seq, total = num_bases,
              min = min_seq, avg = round(num_bases / num_seq), max = max_seq))
}

pretty_output <- function(stats) {
  # Use a dash (-) in place of min, avg, and max if numseq is zero.
  #
  # Args:
  #   stats: The list containing FASTA stats.
  # Returns:
  #   A list containing FASTA stats suitable for output.
  if (stats[["numseq"]] == 0) {
    stats[["min"]] <- "-"
    stats[["avg"]] <- "-"
    stats[["max"]] <- "-"
  }
  return(stats)
}

# Check if all FASTA files exist
exists <- sapply(args$fasta_files, file.exists)
exists[args$fasta_files == 'stdin'] <- TRUE
if (any(! exists)) {
  stop("Files do not exist:\n\t",
       paste(names(exists)[! exists], collapse = "\n\t"))
}

# Check if all FASTA files have read permission
can_read <- file.access(args$fasta_files, mode = 4)
if (any(can_read == -1)) {
  stop("Files cannot be read:\n\t",
       paste(names(can_read)[can_read == -1], collapse = "\n\t"))
}

# Process each FASTA file
results <- lapply(args$fasta_files, FUN = function(x) {
  pretty_output(get_fasta_stats(x, args$min_len))
})

# Convert into table
results <- do.call(rbind, results)
colnames(results) <- toupper(colnames(results))

# Write to stdout
write.table(results, stdout(), sep = "\t", row.names = FALSE, quote = FALSE)
