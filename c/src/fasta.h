
#ifndef _FASTA_H_
#define _FASTA_H_

#include <stdio.h>

struct FastaStats {
    unsigned long total_sequences; // total number of sequences
    unsigned long sequences; // number of sequences of minlength
    unsigned long bases; // total number of bases
    unsigned long min; // min length of sequences
    unsigned long max; // max length of sequences
    double average;
    int is_empty; // file with no content?
};

struct FastaStats processFasta(FILE *fasta_fh, FILE *log, int minlength);

#endif
