#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "fasta_stats.h"
#include <seqan/seq_io.h>
#include <sstream>

void run_test(string input, unsigned int minlen, FastaStats expected)
{
    istringstream stream;
    stream.str(input);
    SeqFileIn seq_file(stream);
    FastaStats result = FastaStats();
    result.from_file(seq_file, minlen);
    REQUIRE( result == expected );
}

TEST_CASE( "one sequence", "[FastaStats]" ) {
    FastaStats expected = FastaStats(1, 5, 5, 5);
    run_test(">header\nATGC\nA", 0, expected);
}

TEST_CASE( "two sequences", "[FastaStats]" ) {
    FastaStats expected = FastaStats(2, 9, 2, 7);
    run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 0, expected);
}

TEST_CASE( "--minlen is less than 2 out of 2 sequences", "[FastaStats]" ) {
    FastaStats expected = FastaStats(2, 9, 2, 7);
    run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 2, expected);
}

TEST_CASE( "--minlen is less than 1 out of 2 sequences", "[FastaStats]" ) {
    FastaStats expected = FastaStats(1, 7, 7, 7);
    run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 3, expected);
}

TEST_CASE( "--minlen is greater than 2 out of 2 sequences", "[FastaStats]" ) {
    FastaStats expected = FastaStats(0, 0, 0, 0);
    run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 8, expected);
}
