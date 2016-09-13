#ifndef BIOTOOL_OPTIONS_H
#define BIOTOOL_OPTIONS_H
using namespace std;

class Options
{
    public:
        unsigned minlen;
        bool verbose;
        bool version;
        vector<string> fasta_files;

        Options(int argc, const char **argv);
};
#endif
