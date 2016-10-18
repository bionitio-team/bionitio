#!/usr/bin/env Rscript

VERSION <- "1.0"
DEFAULT_MIN_LEN <- 0
DEFAULT_VERBOSE <- FALSE

suppressPackageStartupMessages({
  library(optparse, quietly = TRUE)
  library(seqinr, quietly = TRUE)
})

option_list <- list(
  make_option("--minlen",
              help = paste("Minimum length sequence to include in stats",
                           "[default: %default]"),
              type = "integer",
              default = DEFAULT_MIN_LEN),
  make_option("--verbose",
              help = "Print more stuff about what's happening",
              action = "store_true",
              default = DEFAULT_VERBOSE),
  make_option("--version",
              help = "Print version and exit",
              action = "store_true",
              default = FALSE)
)

parser <- OptionParser(                                             # nolint
  usage = paste("%prog [OPTIONS] [FASTA_FILE [FASTA_FILE ...]]",
                "Print FASTA stats.\n",
                "Positional arguments:",
                "\tFASTA_FILE: Input FASTA files. Use - to read from stdin.",
                sep = "\n"),
  option_list = option_list
)

# Parse command line arguments
INVALID_MESSAGE <- "Invalid command line arguments. Use --help for help."
tryCatch({
    suppressWarnings(
      arguments <- parse_args(parser, positional_arguments = TRUE)
    )},
  error = function(e) {
    message(INVALID_MESSAGE)
    quit(status = 2)
  }
)
opts <- arguments$options
args <- arguments$args

# Check options are valid
invalid_options <- sapply(args, function(x) {
  substr(x, 1, 2) == "--"
})
if (any(is.na(opts)) | any(invalid_options)) {
  message(INVALID_MESSAGE)
  quit(status = 2)
}

# Print version
if (opts$version) {
  cat(basename(commandArgs()[4]), VERSION, "\n")
  quit(save = "no")
}

# Read from stdin if argument is '-' or empty
args[args == "-"] <- "stdin"
if (length(args) == 0) {
  args <- "stdin"
}
fasta_files <- args

# Get statistics of a FASTA file
get_fasta_stats <- function(filename, min_len, verbose = FALSE) {
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
      if (verbose) warning(filename, " has no sequences.", call. = FALSE)
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
exists <- sapply(fasta_files, file.exists)
exists[fasta_files == "stdin"] <- TRUE
if (any(! exists)) {
  stop("Files do not exist:\n\t",
       paste(names(exists)[! exists], collapse = "\n\t"))
}

# Check if all FASTA files have read permission
can_read <- file.access(fasta_files, mode = 4)
can_read[fasta_files == "stdin"] <- 0
if (any(can_read == -1)) {
  stop("Files cannot be read:\n\t",
       paste(names(can_read)[can_read == -1], collapse = "\n\t"))
}

# Process each FASTA file
results <- lapply(fasta_files, FUN = function(x) {
  pretty_output(get_fasta_stats(x, opts$minlen, opts$verbose))
})

# Convert into table
results <- do.call(rbind, results)
colnames(results) <- toupper(colnames(results))

# Write to stdout
write.table(results, stdout(), sep = "\t", row.names = FALSE, quote = FALSE)
