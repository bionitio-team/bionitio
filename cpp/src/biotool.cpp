#include <iostream>
#include <seqan/seq_io.h>
#include "options.h"
#include "fasta_stats.h"
#include "constants.h"
#include "exit_status.h"
#include "error.h"

using namespace seqan;
using namespace std;

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
            exit_with_error("Could not open the file.", Error_open_file);
         }
         fasta_stats.from_file(seq_file, options.minlen);
         cout << fasta_stats.pretty(filename) << endl;
      }
   }
}

int main(int argc, const char **argv)
{
    Options options(argc, argv);
    process_files(options);

    return Success;
}
