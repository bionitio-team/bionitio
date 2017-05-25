#ifndef BIOTOOL_OPTIONS_H
#define BIOTOOL_OPTIONS_H

#include <vector>
#include <string>

// Program options set via command line arguments
class Options
{
public:
  // Threshold minimum sequence length. Input sequences with
  // length less than minlen will be ignored
  unsigned minlen;
  // If true, produce more verbose output about program
  // activity
  bool verbose;
  // Print program version number and exit
  bool version;
  // Zero or more input FASTA filenames
  std::vector<std::string> fasta_files;
  // Optional log file. Empty string means no logging should be performed.
  std::string log_filename;
  Options(int argc, const char** argv);
};
#endif
