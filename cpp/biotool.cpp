#include <iostream>
#include <seqan/arg_parse.h>
#include <seqan/seq_io.h>
#include "options.h"

using namespace seqan;
using namespace std;

const char *PROGRAM_NAME = "biotool";
const char *HEADER = "FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX";
const unsigned DEFAULT_MIN_LEN = 0;

typedef enum {Success=0, Error_command_line=1, Error_open_file=2, Error_parse_file=3} exit_status;

void print_error(const char *message)
{
    cerr << PROGRAM_NAME << " ERROR: " << message << endl;
}

class FastaStats {
   private:
      unsigned num_seqs;
      unsigned num_bases;
      unsigned min_len;
      unsigned max_len;
   public:
      void from_file(SeqFileIn &seq_file_in, unsigned minlen_threshold);
      FastaStats (void);
      friend ostream& operator<< (ostream &out, const FastaStats &stats);
      string pretty (string filename);
};

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
           print_error(e.what());
           exit(Error_parse_file);
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

void process_files(Options options)
{
   cout << HEADER << endl;
   FastaStats fasta_stats;

   if (options.fasta_files.size() == 0)
   {
      SeqFileIn seq_file(cin);
      fasta_stats.from_file(seq_file, options.minlen);
      cout << fasta_stats.pretty("stdin") << endl;
   }
   else
   {
      for (string filename : options.fasta_files)
      {
         SeqFileIn seq_file;
         if (!open(seq_file, toCString(filename)))
         {
            print_error("Could not open the file.");
            exit(Error_open_file);
         }
         fasta_stats.from_file(seq_file, options.minlen);
         cout << fasta_stats.pretty(filename) << endl;
      }
   }
}

int main(int argc, const char **argv)
{
    // Read user options
    Options options(argc, argv);
    process_files(options);

    return 0;
}
