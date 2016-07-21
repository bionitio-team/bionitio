#include <iostream>
#include <seqan/seq_io.h>
#include "fasta_stats.h"
#include "error.h"

using namespace seqan;
using namespace std;

FastaStats::FastaStats(void)
{
    num_seqs = 0;
    num_bases = 0;
}

ostream& operator<< (ostream &out, const FastaStats &stats)
{
    out << "FastaStats(" << stats.num_seqs << ", " 
                         << stats.num_bases << ", " 
                         << stats.min_len << "," 
                         << stats.max_len << ")";
    return out;
}

string FastaStats::pretty(string filename)
{
   ostringstream result;

   if(num_seqs > 0)
   {
      unsigned average = num_bases / num_seqs;

      result << filename << "\t" 
             << num_seqs << "\t" 
             << num_bases << "\t"
             << min_len << "\t"
             << average << "\t" 
             << max_len;
   }
   else
   {
      result << filename << "\t0\t0\t-\t-\t-";
   }

   return result.str();
}

void FastaStats::from_file(SeqFileIn &seq_file_in, unsigned minlen_threshold)
{
   unsigned this_len;
   CharString id;
   CharString seq;

   while(!atEnd(seq_file_in))
   {
       try
       {
           readRecord(id, seq, seq_file_in);
       }
       catch (Exception const & e)
       {
	   exit_with_error(e.what(), Error_parse_file);
       }
       this_len = length(seq);
       if (this_len > minlen_threshold)
       {
           if (num_seqs == 0)
           {
              min_len = max_len = this_len;
           }
           else
           {
              min_len = min(min_len, this_len);
              max_len = max(max_len, this_len);
           }
           num_seqs++;
           num_bases += this_len;
       }
   }
}
