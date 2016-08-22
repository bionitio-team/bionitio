
#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

#include "fasta.h"
#include "kseq.h"
#include "log.h"

KSEQ_INIT(gzFile, gzread)

struct FastaStats processFasta(FILE *fh, int verbose, int minlength) {
    struct FastaStats result;
    result.sequences = 0;
    result.bases = 0;
    result.min = 1e9;
    result.max = 0;

    biotool_log(verbose, "opening...");
    gzFile fp = gzdopen(fileno(fh), "r");
    kseq_t *seq = kseq_init(fp);
    biotool_log(verbose, "reading...");
    while(kseq_read(seq) >= 0) {
       // process and/or store the sequences
       result.sequences += 1;
       int bases = strlen(seq->seq.s);
       result.bases += bases;
       result.min = result.min < bases ? result.min : bases;
       result.max = result.max > bases ? result.max : bases;
    }
    if (result.sequences > 0) {
        result.average = (float)result.bases / result.sequences;
    }
    else {
        result.average = 0;
    }
    kseq_destroy(seq);
    biotool_log(verbose, "closing...");
    gzclose(fp);

    return result;
}
