
#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

#include "fasta.h"
#include "kseq.h"
#include "log.h"

KSEQ_INIT(gzFile, gzread)

/**
 * process a single fasta file
 * @param fasta_fh file handle containing fasta file
 * @param log where to write additional progress info
 * @param minlength only process sequences that are at least this length
 * @return structure containing fasta stats
 */
struct FastaStats processFasta(FILE *fasta_fh, FILE* log, int minlength) {
    struct FastaStats result;
    result.total_sequences = 0;
    result.sequences = 0;
    result.bases = 0;
    result.min = 1e9;
    result.max = 0;
    result.is_empty = 0; // not empty

    gzFile fp = gzdopen(fileno(fasta_fh), "r");
    kseq_t *seq = kseq_init(fp);
    while(kseq_read(seq) >= 0) {
        // process and/or store the sequences
        result.total_sequences += 1;
        int bases = strlen(seq->seq.s);
        if (bases >= minlength) {
            result.sequences += 1;
            result.bases += bases;
            result.min = result.min < bases ? result.min : bases;
            result.max = result.max > bases ? result.max : bases;
        }
    }
    result.is_empty = (seq->f->begin == 0);
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
