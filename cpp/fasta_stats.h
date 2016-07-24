#include <seqan/seq_io.h>

using namespace seqan;
using namespace std;

class FastaStats {
   private:
      unsigned int num_seqs;
      unsigned int num_bases;
      unsigned int min_len;
      unsigned int max_len;
   public:
      void from_file(SeqFileIn &seq_file_in, unsigned minlen_threshold);
      FastaStats (void);
      FastaStats (unsigned int num_seqs, unsigned int num_bases, unsigned int min_len, unsigned int max_len);
      friend ostream& operator<< (ostream &out, const FastaStats &stats);
      friend bool operator== (const FastaStats &stats1, const FastaStats &stats2);
      string pretty (string filename);
};
