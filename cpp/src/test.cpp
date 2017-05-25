/*
 * File:       : tests.cpp
 * Description : Unit tests
 * Copyright   : (c) Bernie Pope, 2016
 * License     : MIT
 * Maintainer  : bjpope@unimelb.edu.au
 * Stability   : stable
 * Portability : POSIX
 *
 * Uses the catch library for defining unit tests.
 * */

#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "fasta_stats.h"
#include <seqan/seq_io.h>
#include <sstream>

/*
    Wrapper for running tests. Parse an input string
    as if it were a FASTA file, and compare the actual result
    with what was expected.

    arguments:
        input: string representing the contents of an input test FASTA file
        minlen: threshold minimum sequence length to consider
        expected: the exepcted statistics that should be computed from
            the given input
*/
void
run_test(string input, unsigned int minlen, FastaStats expected)
{
  // Build an input stream from the input string, so it
  // can be treated as if it were an input FASTA file
  istringstream stream;
  stream.str(input);
  SeqFileIn seq_file(stream);
  FastaStats result = FastaStats();
  result.from_file(seq_file, minlen);
  // Check that the actual result is the same as the expected result
  REQUIRE(result == expected);
}

// A FASTA file containing a single sequence
TEST_CASE("one sequence", "[FastaStats]")
{
  FastaStats expected = FastaStats(1, 5, 5, 5);
  run_test(">header\nATGC\nA", 0, expected);
}

// A FASTA file containing two sequences
TEST_CASE("two sequences", "[FastaStats]")
{
  FastaStats expected = FastaStats(2, 9, 2, 7);
  run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 0, expected);
}

// A FASTA file containing two sequences, and minlen less than both of them
TEST_CASE("--minlen is less than 2 out of 2 sequences", "[FastaStats]")
{
  FastaStats expected = FastaStats(2, 9, 2, 7);
  run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 2, expected);
}

// A FASTA file containing two sequences, and minlen less than one of them
TEST_CASE("--minlen is less than 1 out of 2 sequences", "[FastaStats]")
{
  FastaStats expected = FastaStats(1, 7, 7, 7);
  run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 3, expected);
}

// A FASTA file containing two sequences, and minlen greater than both of them
TEST_CASE("--minlen is greater than 2 out of 2 sequences", "[FastaStats]")
{
  FastaStats expected = FastaStats(0, 0, 0, 0);
  run_test(">header1\nATGC\nAGG\n>header2\nTT\n", 8, expected);
}
