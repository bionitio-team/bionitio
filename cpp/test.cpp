#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "fasta_stats.h"
#include <seqan/seq_io.h>
#include <sstream>

TEST_CASE( "one sequence", "[FastaStats]" ) {
    istringstream stream;
    stream.str(">header\nATGC\nA");
    SeqFileIn seq_file(stream);
    FastaStats result = FastaStats();
    result.from_file(seq_file, 0);
    FastaStats expected = FastaStats(1, 5, 5, 5);
    REQUIRE( result == expected );
}
