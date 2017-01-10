/*
File:       : options.cpp 
Description : Compute various statistics about a FASTA file. 
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : stable 
Portability : POSIX

Command line argument processing.
*/

#include <boost/program_options.hpp>
#include <iostream>
#include "constants.h"
#include "options.h"
#include "error.h"
#include "exit_status.h"

namespace po = boost::program_options;
using namespace std;

/*
    Process command line arguments from the argument vector.
    Store the results in an Options object. 

    The program will exit with an error if the command line
    cannot be successfully parsed.

    The program will also exit if the user requests the -h (help)
    argument, or -v (version)

    arguments:
        argc: argument count, the number of strings on the command line 
        argv: argument vector, an array of argc number of strings,
            containing all the strings specified on the command line
    result:
        An Options object, whose attributes represent the values
            of command line arguments
*/
Options::Options(int argc, const char** argv)
{
    minlen = DEFAULT_MIN_LEN;
    verbose = false;
    version = false;
    log_filename = "";
    fasta_files = {};

    string minlen_str =
        "Minimum length sequence to include in stats (default " +
        to_string(DEFAULT_MIN_LEN) + ")";
    string version_str = "Display program version and exit";
    string verbose_str = "Print more stuff about what's happening";
    string log_filename_str = "Log file name";

    // Description of the command line arguments.
    po::options_description desc(PROGRAM_NAME + " allowed options");
    desc.add_options()
        ("help,h", "Show this help information.")
        ("minlen,m", po::value<unsigned int>(), minlen_str.c_str())
        ("verbose", verbose_str.c_str())
        ("version", version_str.c_str())
	("log", po::value<string>(), log_filename_str.c_str())
        ("files",
            po::value<vector<string>>()->multitoken()->zero_tokens()->composing(),
            "FASTA FILES");

    po::positional_options_description pos_desc;
    pos_desc.add("files", -1);
    po::command_line_parser parser{argc, argv};
    parser.options(desc).positional(pos_desc);
    po::variables_map vm;

    // Try to parse the command line, and handle errors that occur.
    try
    {
        po::parsed_options parsed_options = parser.run();
        store(parsed_options, vm);
    }
    catch(boost::program_options::required_option& e)
    {
        exit_with_error(e.what(), Error_command_line);
    }
    catch(boost::program_options::error& e)
    {
        exit_with_error(e.what(), Error_command_line);
    }

    // If the user requested the -h (help) flag, print a
    // usage message and exit
    if (vm.count("help"))
    {
        cout << "Usage:\n\n" << desc << "\n";
        exit(Success);
    }
    // Look for each of the arguments, and set up the attributes of
    // the Options object appropriately.
    if (vm.count("minlen"))
    {
        minlen = vm["minlen"].as<unsigned int>();
    }
    if (vm.count("verbose"))
    {
        verbose = true;
    }
    if (vm.count("version"))
    {
        version = true;
    }
    if (vm.count("log"))
    {
        log_filename = vm["log"].as<string>();
    }
    if (vm.count("files"))
    {
        fasta_files = vm["files"].as<vector<string>>();
    }
}
