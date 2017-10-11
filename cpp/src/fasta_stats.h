#ifndef BIONITIO_FASTA_STATS_H
#define BIONITIO_FASTA_STATS_H

#include <seqan/seq_io.h>

using namespace seqan;
using namespace std;

class FastaStats
{
  // Note that, we filter out sequences whose length is less than the
  // threshold specified on the command line. Therefore, the number
  // of sequences considered in computing the statistics may be fewer
  // than the true total number of sequences in the file.
private:
  // Number of sequences considered in the FASTA file
  unsigned int num_seqs;
  // Total number of bases in all of the considered sequences in a FASTA file
  unsigned int num_bases;
  // Minimum length of all the sequences considered in a FASTA file
  unsigned int min_len;
  // Maximum length of all the sequences considered in a FASTA file
  unsigned int max_len;

public:
  // Read an input FASTA file and compute the necessary statistics
  void from_file(SeqFileIn& seq_file_in, unsigned minlen_threshold);
  // Build and empty FastaStats object
  FastaStats(void);
  // Build a FastaStats object using supplied attribute values
  FastaStats(unsigned int num_seqs, unsigned int num_bases,
             unsigned int min_len, unsigned int max_len);
  // Render FastaStats on an output stream
  friend ostream& operator<<(ostream& out, const FastaStats& stats);
  // Compare two FastaStats objects for equality
  friend bool operator==(const FastaStats& stats1, const FastaStats& stats2);
  // Pretty print a FastaStats object as a string
  string pretty(string filename);
};
#endif
