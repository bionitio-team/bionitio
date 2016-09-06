
// a simple unit testing methodology based on assert

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

#include "fasta.h"

const char *TMPFILE="tmp-biotool-test";

static void createFile(const char *data) {
    FILE *tmp = fopen(TMPFILE, "w");
    fprintf(tmp, "%s", data);
    fclose(tmp);
}

static void testOneSequence() {
    createFile(">header\nATGC\nA");
    struct FastaStats result = processFasta(fopen(TMPFILE, "r"), 0, 0);
    assert(result.sequences == 1 && "testOneSequence.sequences");
    assert(result.bases == 5 && "testOneSequence.bases");
    assert(result.min == 5 && "testOneSequence.min");
    assert(result.max == 5 && "testOneSequence.max");
}

static void testTwoSequences() {
    createFile(">header1\nATGC\nAGG\n>header2\nTT\n");
    struct FastaStats stats = processFasta(fopen(TMPFILE, "r"), 0, 0);
    assert(stats.sequences == 2 && "testTwoSequences.sequences");
    assert(stats.bases == 9 && "testTwoSequences.bases");
    assert(stats.min == 2 && "testTwoSequences.min");
    assert(stats.max == 7 && "testTwoSequences.max");
}

static void testMinLen() {
    createFile(">header1\nATGC\nAGG\n>header2\nTT\n");
    struct FastaStats stats = processFasta(fopen(TMPFILE, "r"), 0, 3);
    assert(stats.sequences == 1 && "testMinLen.sequences");
    assert(stats.bases == 7 && "testMinLen.bases");
    assert(stats.min == 7 && "testMinLen.min");
    assert(stats.max == 7 && "testMinLen.max");
}

int main() {
    testOneSequence();
    testTwoSequences();
    testMinLen();

    unlink(TMPFILE);
    
    return 0; // success
}
