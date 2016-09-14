#include <boost/program_options.hpp>
#include <iostream>
#include "constants.h"
#include "options.h"
#include "error.h"
#include "exit_status.h"

namespace po = boost::program_options;
using namespace std;

Options::Options(int argc, const char** argv)
{
    minlen = DEFAULT_MIN_LEN;
    verbose = false;
    version = false;
    fasta_files = {};

    string minlen_str =
        "Minimum length sequence to include in stats (default " +
        to_string(DEFAULT_MIN_LEN) + ")";
    string version_str = "Display program version and exit";
    string verbose_str = "Print more stuff about what's happening";

    po::options_description desc(PROGRAM_NAME + " allowed options");
    desc.add_options()
        ("help,h", "Show this help information.")
        ("minlen,m", po::value<unsigned int>(), minlen_str.c_str())
        ("verbose", verbose_str.c_str())
        ("version", version_str.c_str())
        ("files",
            po::value<vector<string>>()->multitoken()->zero_tokens()->composing(),
            "FASTA FILES");

    po::positional_options_description pos_desc;
    pos_desc.add("files", -1);
    po::command_line_parser parser{argc, argv};
    parser.options(desc).positional(pos_desc);
    po::variables_map vm;

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

    if (vm.count("help"))
    {
        cout << "Usage:\n\n" << desc << "\n";
        exit(Success);
    }
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
    if (vm.count("files"))
    {
        fasta_files = vm["files"].as<vector<string>>();
    }
}
