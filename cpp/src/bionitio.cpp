/*
File:       : bionitio.cpp
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

#include "constants.h"
#include "error.h"
#include "exit_status.h"
#include "fasta_stats.h"
#include "options.h"
#include <boost/algorithm/string/join.hpp>
#include <boost/log/core.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/sinks/text_file_backend.hpp>
#include <boost/log/sources/record_ostream.hpp>
#include <boost/log/sources/severity_logger.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup/common_attributes.hpp>
#include <boost/log/utility/setup/file.hpp>
#include <iostream>
#include <seqan/seq_io.h>
#include <sys/time.h>

using namespace seqan;
using namespace std;
namespace logging = boost::log;
namespace src = boost::log::sources;
namespace sinks = boost::log::sinks;
namespace keywords = boost::log::keywords;

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
void
process_files(Options options)
{
  cout << HEADER << endl;
  FastaStats fasta_stats;

  // Check how many files were specified on the command line.
  if (options.fasta_files.size() == 0) {
    BOOST_LOG_TRIVIAL(info) << "Processing FASTA file from stdin";
    // Try to read a FASTA file from stdin
    SeqFileIn seq_file(cin);
    // Compute various statistics for the file
    fasta_stats.from_file(seq_file, options.minlen);
    // Pretty print the results
    cout << fasta_stats.pretty("stdin") << endl;
  } else {
    // Process each file specified on the command line
    for (string filename : options.fasta_files) {
      BOOST_LOG_TRIVIAL(info) << "Processing FASTA file from: " << filename;
      ifstream input_file(filename);
      if (!input_file) {
        exit_with_error("Could not open the file: " + filename,
                        Error_open_file);
      }
      SeqFileIn seq_file(input_file);
      // Compute various statistics for the file
      fasta_stats.from_file(seq_file, options.minlen);
      // Pretty print the results
      cout << fasta_stats.pretty(filename) << endl;
    }
  }
}

void
init_logging(Options options, std::string command_line)
{

  if (options.log_filename.size() > 0) {
    logging::add_common_attributes();
    boost::log::register_simple_formatter_factory<
      boost::log::trivial::severity_level, char>("Severity");
    boost::log::add_file_log(
      boost::log::keywords::file_name = options.log_filename,
      boost::log::keywords::format = "[%TimeStamp%] (%Severity%): %Message%");
    BOOST_LOG_TRIVIAL(info) << "program started";
    BOOST_LOG_TRIVIAL(info) << "command line: " << command_line;
  } else {
    // turn off logging
    boost::shared_ptr<logging::core> core = logging::core::get();
    core->set_logging_enabled(false);
  }
}

/*
    Entry point for the program
        - parse command line options
        - optionally initialise logging
        - process input files

    arguments:
        argc: the number of strings in the argument vector
        argv: the argument vector, an array of strings
*/
int
main(int argc, const char** argv)
{
  Options options(argc, argv);
  std::vector<std::string> arguments(argv + 1, argv + argc);
  std::string command_line = boost::algorithm::join(arguments, " ");
  init_logging(options, command_line);
  process_files(options);
  return Success;
}
