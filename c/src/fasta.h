
#ifndef _FASTA_H_
#define _FASTA_H_

#include <stdio.h>

struct FastaStats {
    unsigned long sequences;
    unsigned long bases;
    unsigned long min;
    unsigned long max;
    double average;
};

struct FastaStats processFasta(FILE *fh, int verbose, int minlength);

#endif
