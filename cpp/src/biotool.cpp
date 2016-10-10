/*
File:       : biotool.cpp 
Description : The main entry point for the program.
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : stable 
Portability : POSIX

This module orchestrates the overall behaviour of the program. The main
parts are:
 * Parse command line arguments. 
 * Process each FASTA file in sequence.
 * Pretty print output. 
*/    

#include <iostream>
#include <seqan/seq_io.h>
#include "options.h"
#include "fasta_stats.h"
#include "constants.h"
#include "exit_status.h"
#include "error.h"

using namespace seqan;
using namespace std;

/*
    Process each input FASTA file.
        - print the output header
        - check if any FASTA files were given on the command line
        - if zero FASTA files, read from stdin

    arguments:
        options: the command line options of the program
    result:
        void
*/
void process_files(Options options)
{
    cout << HEADER << endl;
    FastaStats fasta_stats;

    // Check how many files were specified on the command line.
    if (options.fasta_files.size() == 0)
    {
        // Try to read a FASTA file from stdin
        SeqFileIn seq_file(cin);
        // Compute various statistics for the file
        fasta_stats.from_file(seq_file, options.minlen);
        // Pretty print the results
        cout << fasta_stats.pretty("stdin") << endl;
    }
    else
    {
        // Process each file specified on the command line
        for (string filename : options.fasta_files)
        {
            ifstream input_file(filename);
            if (!input_file)
            {
                exit_with_error("Could not open the file.", Error_open_file);
            }
            SeqFileIn seq_file(input_file);
            // Compute various statistics for the file
            fasta_stats.from_file(seq_file, options.minlen);
            // Pretty print the results
            cout << fasta_stats.pretty(filename) << endl;
        }
    }
}

/*
    Entry point for the program
        - parse command line options
        - process input files

    arguments:
        argc: the number of strings in the argument vector
        argv: the argument vector, an array of strings
*/
int main(int argc, const char **argv)
{
    Options options(argc, argv);
    process_files(options);
    return Success;
}
