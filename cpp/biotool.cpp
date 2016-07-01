#include <iostream>
#include <seqan/arg_parse.h>
#include <seqan/seq_io.h>

using namespace seqan;
using namespace std;

const char *PROGRAM_NAME = "biotool";
const char *HEADER = "FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX";
const unsigned DEFAULT_MIN_LEN = 0;

typedef enum {Success=0, Error_command_line=1, Error_open_file=2, Error_parse_file=3} exit_status;

struct BiotoolOptions
{
    unsigned minlen;
    bool verbose;
    bool version;
    vector<string> fasta_files;

    BiotoolOptions() :
    minlen(DEFAULT_MIN_LEN), verbose(false), version(false), fasta_files({})
    {}
};

void print_error(const char *message)
{
    cerr << PROGRAM_NAME << " ERROR: " << message << endl;
}

BiotoolOptions parse_options(int argc, char const **argv)
{
    ArgumentParser parser(PROGRAM_NAME);
    addArgument(parser, ArgParseArgument(
        ArgParseArgument::STRING, "FASTA_FILE", true));
    addOption(parser, ArgParseOption(
        "m", "minlen", "Minimum length sequence to include in stats (default " + to_string(DEFAULT_MIN_LEN) + ")",
        ArgParseArgument::INTEGER, "INT"));
    addOption(parser, ArgParseOption(
        "v", "version", "Display program version and exit"));
    addOption(parser, ArgParseOption(
        "b", "verbose", "Print more stuff about what's happening"));
    ArgumentParser::ParseResult res = parse(parser, argc, argv);

    //if (res == ArgumentParser::PARSE_ERROR)
    if (res != ArgumentParser::PARSE_OK)
    {
	if (res == ArgumentParser::PARSE_ERROR) { 
           exit(Error_command_line);
	}
	else {
           exit(Success);
        }
    }

    BiotoolOptions options;
    getOptionValue(options.minlen, parser, "minlen");
    options.verbose = isSet(parser, "verbose");
    options.version = isSet(parser, "version");
    options.fasta_files = getArgumentValues(parser, 0);
    return options;
}

void process_files(BiotoolOptions options)
{
    CharString id;
    CharString seq;

    cout << HEADER << endl;

    for (string filename : options.fasta_files) {
	SeqFileIn seqFileIn;

        if (!open(seqFileIn, toCString(filename)))
        {
	    print_error("Could not open the file.");
            exit(Error_open_file);
        }

        int max_len = 0;
        int min_len = -1; 
	int this_len;
        unsigned total_len = 0; 
        unsigned num_seqs = 0;
	unsigned int average;

        while(!atEnd(seqFileIn))
        {
	    try
	    {
	        readRecord(id, seq, seqFileIn);
	    }
            catch (Exception const & e)
            {
	        print_error(e.what());
                exit(Error_parse_file);
            }
	    this_len = length(seq);
	    if (this_len > options.minlen)
            {
	        num_seqs++;
	        total_len += this_len;
	        max_len = max(max_len, this_len);
	        if (min_len < 0 || this_len < min_len)
                {
                    min_len = this_len;
                }
	    }
        }
	if (num_seqs > 0)
        {
            average = (unsigned int) floor((double) total_len / (double) num_seqs);
	    cout << filename << '\t' << num_seqs << '\t' \
		 << min_len << '\t' << average << '\t' << max_len << endl;
        }
	else
        {
	    cout << filename << "\t0\t0\t-\t-\t-" << endl;
        }
    }
}

int main(int argc, char const **argv)
{
    BiotoolOptions options = parse_options(argc, argv);
    process_files(options);

    return 0;
}
