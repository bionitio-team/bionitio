
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

    gzFile fp = gzdopen(fileno(fh), "r");
    kseq_t *seq = kseq_init(fp);
    while(kseq_read(seq) >= 0) {
       // process and/or store the sequences
       int bases = strlen(seq->seq.s);
       if (bases >= minlength) {
           result.sequences += 1;
           result.bases += bases;
           result.min = result.min < bases ? result.min : bases;
           result.max = result.max > bases ? result.max : bases;
       }
    }
    if (result.sequences > 0) {
        result.average = (float)result.bases / result.sequences;
    }
    else {
        result.average = 0;
    }
    kseq_destroy(seq);
    gzclose(fp);

    return result;
}
