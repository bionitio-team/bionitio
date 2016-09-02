#!/usr/bin/env Rscript

version <- "1.0"

suppressWarnings({
  library(argparse, quietly=TRUE)
  library(seqinr, quietly=TRUE)
})

parser <- ArgumentParser(description="Print FASTA stats")
parser$add_argument("fasta_files", metavar="FASTA_FILE", type="character", nargs="?", default="stdin",
                    help="Input FASTA files")
parser$add_argument("--minlen", metavar="N", type="integer", dest="min_len", default=0,
                    help="Minimum length sequence to include in stats [default: %(default)s]")
parser$add_argument("--verbose", dest="verbose", action="store_true",
                    help="Print more stuff about what's happening")
parser$add_argument("--version", dest="print_version", action="store_true",
                    help="Print version and exit")

# Print version
if ("--version" %in% commandArgs()) {
  cat(basename(commandArgs()[4]), version, "\n")
  quit(save="no")
}

# Process command line arguments
args <- parser$parse_args()

# Get statistics of a FASTA file
get_fasta_stats <- function(filename, min_len) {
  min_seq <- Inf
  max_seq <- 0
  num_seq <- 0
  num_bases <- 0

  sequences <- tryCatch(
    read.fasta(file=filename, seqtype="AA", seqonly=TRUE),
    error=function(e) {
      if (args$verbose) warning(e, filename, " has no sequences.", call.=FALSE)
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
  return(list(filename=filename, total=num_bases, numseq=num_seq, min=min_seq,
              avg=round(num_bases/num_seq), max=max_seq))
}


# Check if FASTA files exist
exists <- sapply(args$fasta_files, file.exists)
exists <- args$fasta_files == 'stdin'
if (any(! exists)) {
  stop("File does not exist:\n\t", paste(args$fasta_files[! exists], collapse="\n\t"))
}
fasta_files <- args$fasta_files[exists]

# Process each FASTA file
results <- lapply(fasta_files, FUN=function(x){get_fasta_stats(x, args$min_len)})
results <- do.call(rbind, results)
colnames(results) <- toupper(colnames(results))

# Write to stdout
write.table(results, stdout(), sep="\t", row.names=FALSE, quote=FALSE)
