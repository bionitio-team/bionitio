#ifndef BIONITIO_EXIT_STATUS_H
#define BIONITIO_EXIT_STATUS_H
// Program exit status values
typedef enum {
  Success = 0 // Program succeeded
  ,
  Error_open_file = 1 // Error opening an input FASTA file
  ,
  Error_command_line = 2 // Error on the command line arguments
  ,
  Error_parse_file = 3 // Error parsing an input FASTA file
} exit_status;
#endif
