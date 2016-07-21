#include <seqan/seq_io.h>

using namespace seqan;
using namespace std;

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
