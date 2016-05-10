#include <iostream>
#include <seqan/arg_parse.h>

#define DEFAULT_MIN_LEN 0

struct BiotoolOptions
{
    unsigned minlen;
    bool verbose;
    bool version;
    //seqan::CharString text;

    BiotoolOptions() :
    minlen(DEFAULT_MIN_LEN), verbose(false), version(false)
    {}
};

int main(int argc, char const ** argv)
{
    // Setup ArgumentParser.
    seqan::ArgumentParser parser("Print fasta stats");

    //addArgument(parser, seqan::ArgParseArgument(
    //    seqan::ArgParseArgument::STRING, "TEXT"));

    addOption(parser, seqan::ArgParseOption(
        "m", "minlen", "Minimum length sequence to include in stats (default " + std::to_string(DEFAULT_MIN_LEN) + ")",
        seqan::ArgParseArgument::INTEGER, "INT"));

    addOption(parser, seqan::ArgParseOption(
        "v", "version", "Display program version and exit"));

    addOption(parser, seqan::ArgParseOption(
        "b", "verbose", "Print more stuff about what's happening"));

    // Parse command line.
    seqan::ArgumentParser::ParseResult res = seqan::parse(parser, argc, argv);

    // If parsing was not successful then exit with code 1 if there were errors.
    // Otherwise, exit with code 0 (e.g. help was printed).
    if (res != seqan::ArgumentParser::PARSE_OK)
        return res == seqan::ArgumentParser::PARSE_ERROR;


    BiotoolOptions options;

    getOptionValue(options.minlen, parser, "minlen");
    options.verbose = isSet(parser, "verbose");
    options.version = isSet(parser, "version");
    // getArgumentValue(options.text, parser, 0);


    // Extract option values and print them.
    /*
    unsigned period = 0;
    getOptionValue(period, parser, "period");
    bool toUppercase = isSet(parser, "uppercase");
    seqan::CharString text;
    getArgumentValue(text, parser, 0);

    std::cout << "period   \t" << period << '\n'
              << "uppercase\t" << toUppercase << '\n'
              << "text     \t" << text << '\n';
    */

    return 0;
}
