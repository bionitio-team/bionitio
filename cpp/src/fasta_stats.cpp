/*
File:       : fasta_stats.cpp 
Description : Compute various statistics about a FASTA file. 
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : stable 
Portability : POSIX

Implementation of the FastaStats class, for reading sequences from
FASTA files, computing statistics about the contents of the file,
and representing the results.
*/

#include <iostream>
#include <seqan/seq_io.h>
#include "fasta_stats.h"
#include "error.h"

using namespace seqan;
using namespace std;

/*
    Construct an initial empty FastaStats object. 
    All counters set to zero.
*/
FastaStats::FastaStats(void)
{
    num_seqs = 0;
    num_bases = 0;
    min_len = 0;
    max_len = 0;
}

/*
    Construct an initial non-empty FastaStats object. 
    All counters set to their corresponding parameters.

    arguments:
        ns: number of sequences in FASTA file
        nb: number of bases in FASTA file over all sequences
        mnl: minimum length of a sequence in the FASTA file
        mxl: maximum length of a sequence in the FASTA file
*/
FastaStats::FastaStats (unsigned int ns, unsigned int nb, unsigned int mnl, unsigned int mxl):
    num_seqs(ns), num_bases(nb), min_len(mnl), max_len(mxl) {}

/*
    Compare two FastaStats objects for equality. 
    The objects are considered equal if all their attributes are equal.

    arguments:
        stats1: reference to first FastaStats object
        stats2: reference to second FastaStats object
    result:
        boolean, true if the two input FastaStats objects are equal, false
        otherwise
*/
bool operator== (const FastaStats &stats1, const FastaStats &stats2)
{
    return stats1.num_seqs == stats2.num_seqs &&
           stats1.num_bases == stats2.num_bases &&
           stats1.min_len == stats2.min_len &&
           stats1.max_len == stats2.max_len;
}

/*
    Print a FastaStats object on an output stream.

    arguments:
        out: reference to output stream 
        stats: reference to a FastaStats object to print
    result:
       output stream, useful for chaining output expressions.
*/
ostream& operator<< (ostream &out, const FastaStats &stats)
{
    out << "FastaStats("
	<< stats.num_seqs << ", " 
        << stats.num_bases << ", " 
        << stats.min_len << ", " 
        << stats.max_len << ")";
    return out;
}

/*
    Render a FastaStats object as a string in a format required
    by biotool. The average length of sequences is computed at this
    point, and included in the output.

    The fields in the output are tab separated.

    If the number of counted sequences is not > 0, num_seqs and
    num_bases are displayed as 0, and the remaining fields are
    rendered as dashes (-) to indicate that their values are not
    available.

    arguments:
        filename: identifier for the file (or stream) from which the
            FASTA object was read (could be a filename or "stdin") 
    result:
       string: rendering of the FastaStats object 
*/
string FastaStats::pretty(string filename)
{
    ostringstream result;

    if(num_seqs > 0)
    {
        // Average sequence length, rounded down towards 0
        unsigned average = num_bases / num_seqs;

        result << filename << "\t" 
               << num_seqs << "\t" 
               << num_bases << "\t"
               << min_len << "\t"
               << average << "\t" 
               << max_len;
    }
    else
    {
        result << filename << "\t0\t0\t-\t-\t-";
    }

    return result.str();
}

/*
    
    Compute FastaStats from an input file.

    arguments:
        seq_file_in: reference to input FASTA file, as SeqFileIn from seqan
            library
        minlen_threshold: minimum length of sequence to be considered for
            counting. Sequenecs with lengths strictly below the threshold will
            be ignored, and not included in the resulting statistics.
    result:
       FastaStats object representing the computed statistics 
*/
void FastaStats::from_file(SeqFileIn &seq_file_in, unsigned minlen_threshold)
{
    unsigned this_len;
    CharString id;
    CharString seq;

    // Read all sequences from the input FASTA file, one at a time
    while(!atEnd(seq_file_in))
    {
        // Parse the next sequence header and value
        try
        {
            readRecord(id, seq, seq_file_in);
        }
        // Exit if any parse errors are encountered
        catch (Exception const &e)
        {
	    exit_with_error(e.what(), Error_parse_file);
        }
        // Skip sequences whose length is less than threshold 
        this_len = length(seq);
        if (this_len >= minlen_threshold)
        {
            // We haven't seen any sequences yet, so initialise
            // min_len and max_len to the length of this sequence
            if (num_seqs == 0)
            {
                min_len = max_len = this_len;
            }
            // We seen at least one sequence before, update
            // min_len and max_len
            else
            {
                min_len = min(min_len, this_len);
                max_len = max(max_len, this_len);
            }
            // Update sequence count and base count
            num_seqs++;
            num_bases += this_len;
        }
    }
}
