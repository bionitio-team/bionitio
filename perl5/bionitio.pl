#!/usr/bin/env perl

# Module      : bionitio.pl
# Description : The main entry point for the program.
# Copyright   : (c) Torsten Seemann and Bernie Pope, 2016 - 2017
# License     : MIT
# Maintainer  : tseemann@unimelb.edu.au
# Portability : POSIX
#
# The program reads one or more input FASTA files. For each file it computes a
# variety of statistics, and then prints a summary of the statistics as output.
#
# The main parts of the program are:
# 1. Parse command line arguments.
# 2. Process each FASTA file in sequence.
# 3. Pretty print output for each file.
#
# If no FASTA filenames are specified on the command line then the program
# will try to read a FASTA file from standard input.

use strict;
use warnings 'FATAL' => 'all';
use File::Spec;
use Bio::SeqIO;
use Bio::Root::Exception;
use Log::Log4perl qw(get_logger :nowarn);
use Getopt::ArgParse;
use Try::Tiny;
use Readonly;
use version; our $VERSION = qv('1.0');

# Exit status codes.
# 0: Success
# 1: File I/O error. This can occur if at least one of the input FASTA
#        files cannot be opened for reading. This can occur because the file
#        does not exist at the specified path, or bionitio does not have permission
#        to read from the file.
# 2: A command line error occurred. This can happen if the user specifies
#        an incorrect command line argument. In this circumstance bionitio will
#        also print a usage message to the standard error device (stderr).
# 3: FASTA file error. This can occur when the input FASTA file is
#        incorrectly formatted, and cannot be parsed.
Readonly my $EXIT_SUCCESS            => 0;
Readonly my $EXIT_FILE_IO_ERROR      => 1;
Readonly my $EXIT_COMMAND_LINE_ERROR => 2;
Readonly my $EXIT_FASTA_FILE_ERROR   => 3;

# Program version number
#Readonly my $VERSION => '1.0';

# Default value for the minlen command line argument
Readonly my $DEFAULT_MINLEN => 0;

# Header row for the output
Readonly my $HEADER => "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX";

# Get the program name
my ( undef, undef, $PROGRAM_NAME ) = File::Spec->splitpath($0);

# Log message handler. This is global so that we can refer to it
# everywhere in the program without having to pass it as a parameter.
my $logger;

# Initialise program logging
#
# Arguments:
#     logfile: the filename (string) of the output log file, where log messages
#         will be written. If this is undefined then the logger will not
#         be configured, and no log messages will be written (even when the
#         logging functions are called).
# Result: None
#
# Initial log messages are written indicating that the program has started, and
# the command line arguments that were used.
sub init_logging {

    # The output log message pattern is:
    #     %d date (and time), %p priority (level), %m message, %n newline
    # The minimum priority is INFO.

    my ($logfile) = @_;

    # Create a logging handler
    if ($logfile) {
        my $log_conf = qq(
      log4perl.logger                    = INFO, FileApp
      log4perl.appender.FileApp          = Log::Log4perl::Appender::File
      log4perl.appender.FileApp.filename = $logfile 
      log4perl.appender.FileApp.layout   = PatternLayout
      log4perl.appender.FileApp.layout.ConversionPattern = %d %p %m%n
    );
        Log::Log4perl->init( \$log_conf );
    }

    # Obtain a logger instance
    $logger = get_logger('Bionitio');
    $logger->info('program started');

    # Log the program name and command line arguments
    $logger->info("command line arguments: $0 @ARGV");
    return;
}

# Print an error message and exit the program.
#
# Arguments:
#     message: an error message as a string.
#     exit_status: a positive integer representing the exit status of the
#         program.
#
# Result: None
#
# Print an error message to stderr, prefixed by the program name and 'ERROR'.
# Then exit program with supplied exit status. If logging is enabled, the error
# message is also written to the log file.
sub exit_with_error {
    my ( $message, $exit_status ) = @_;
    $logger->error($message);
    print {*STDERR} "$PROGRAM_NAME ERROR: $message\n";
    exit $exit_status;
}

# Collect statistics from a single input FASTA file.
#
# Arguments:
#     filename: a descriptive name of the input FASTA file, can be
#         any string, this is only used to generate error messages.
#     filehandle: open file handle representing the input FASTA file
#     minlen_threshold: the minimum length sequence in the FASTA file
#         that will be considered when computing the statistics.
#         Sequences shorter than this length will be ignored and skipped
#         over.
#
# Result: A list of five values representing:
#     - the number of sequences counted in the input file
#     - the total number of bases in all of the counted sequences
#     - the length of the shortest counted sequence
#     - the average length of the counted sequences rounded down to the
#         nearest integer.
#     - the length of the longest counted sequence
#
# If the input file is empty, or no sequences are considered (because
# none satisfied the minimum length threshold), then the first two values
# in the output list (number of sequences, and total number of bases) will
# be set to 0. The remaining values cannot be computed, so are represented
# by a single-dash character in a string '-'.
#
# If there is an error parsing the input FILE then we exit the program
# with an error message.
sub process_file {
    my ( $filename, $filehandle, $minlen_threshold ) = @_;

    # to collect stats
    my $num_bases = 0;
    my $num_seqs  = 0;
    my $min_len   = undef;
    my $max_len   = undef;

    try {
        my $in = Bio::SeqIO->new( -fh => $filehandle, -format => 'fasta' );

        # Process each sequence in the input FASTA file
        while ( my $seq = $in->next_seq ) {
            my $this_len = $seq->length;

            # Skip this sequence if its length is less than the threshold
            if ( $this_len >= $minlen_threshold ) {
                $num_seqs++;
                $num_bases += $this_len;
                if ( !defined($min_len) || $this_len < $min_len ) {
                    $min_len = $this_len;
                }
                if ( !defined($max_len) || $this_len > $max_len ) {
                    $max_len = $this_len;
                }
            }
        }
    }
    catch {
        exit_with_error(
            "An error occurred when reading the FASTA file from $filename:\n$_",
            $EXIT_FASTA_FILE_ERROR
        );
    };

    # Check if any sequences were counted
    return $num_seqs <= 0
      ? [ $num_seqs, $num_bases, q{-}, q{-}, q{-} ]
      : [
        $num_seqs, $num_bases,
        $min_len,  int( $num_bases / $num_seqs ),
        $max_len
      ];
}

# Compute statistics for each input FASTA file, or from standard input
#
# Arguments:
#     options: the command line options given to the program, specified as a hash
#
# Result: None
#
# If the user specified one or more named FASTA files on the command line, then each
# file is processed in-turn. If an error occurs trying to process a given file
# the program exits immediately, and does not try to process any remaining files.
# If no FASTA files are named, then the program tries
# to read a FASTA file from the standard input device.
sub process_files {
    my ($options) = @_;

    print "$HEADER\n";

    if ( scalar @{ $options->fasta_files } ) {

        # Process each named FASTA file in-turn
        for my $filename ( $options->fasta_files ) {
            $logger->info("Processing FASTA file from $filename");
            if ( open my $filehandle, '<', $filename ) {
                my $res =
                  process_file( $filename, $filehandle, $options->minlen );
                if ($res) { print pretty_output( $filename, $res ); }
		# XXX why does this close fail?
		# close $filehandle;
            }
            else {
                exit_with_error( "Could not open $filename for reading",
                    $EXIT_FILE_IO_ERROR );
            }
        }
    }
    else {
        # Try to read and process a FASTA file from the standard input device
        $logger->info('Processing FASTA file from stdin');
        my $result = process_file( 'stdin', \*STDIN, $options->minlen );
        if ($result) { print pretty_output( 'stdin', $result ); }
    }
    return;
}

# Pretty print the FASTA statistics as a tab-separated string
#
# Arguments:
#     filename: the name of the input FASTA file (or "stdin" if the input was
#         read from standard input)
#     statistics: the computed statistics for this particular FASTA file, as a list
#         of values
#
# Result: a string in tab separated format, with the filename prepended onto the
#     front of the statistics
sub pretty_output {
    my ( $filename, $statistics ) = @_;
    return "$filename\t" . join( "\t", @{$statistics} ) . "\n";
}

# Parse command line arguments
#
# Arguments: None
# Result: Returns a object containing the specified command line arguments
#     as attributes.
#
# If the user supplies the --help (or -h) option, this function will print
# a usage message, and then exit the program successfully.
# If the user supplies the --version option, this function will print the program
# name a version number, and exit the programm successfully.
# If the user invokes the program with incorrect arguments then this function
# will print a usage message and exit the program with an error.
sub get_options {
    my $parser = Getopt::ArgParse->new_parser(
        prog => 'bionitio.pl',
        description =>
          'Read one or more FASTA files, compute simple stats for each file',
    );
    $parser->add_arg(
        '--minlen', '-m',
        type    => 'Scalar',
        default => $DEFAULT_MINLEN,
        help    => 'Minimum length sequence to include in stats',
        metavar => 'N'
    );
    $parser->add_arg(
        '--version',
        type => 'Bool',
        help => 'Print the program version and then exit'
    );
    $parser->add_arg(
        '--log',
        type    => 'Scalar',
        help    => 'record program progress in LOG_FILE',
        metavar => 'LOG_FILE'
    );
    $parser->add_arg(
        'fasta_files',
        help    => 'input FASTA files',
        nargs   => q{*},
        type    => 'Array',
        metavar => 'FASTA_FILES'
    );
    return $parser->parse_args();
}

# The entry point for the program
#
# Arguments: None
# Result: None
#
# This function controls the overall execution of the program
sub main {
    my $options = get_options();
    if ( $options->version ) {
        print "$PROGRAM_NAME version $VERSION\n";
        exit $EXIT_SUCCESS;
    }
    init_logging( $options->log );
    process_files($options);
    exit $EXIT_SUCCESS;
}

# Run the main function only if this script has not been loaded
# from another script. This is useful for unit testing. If the
# script is run from the command line, then the main function will
# be executed, and the program will run as normal. However, if the script
# is imported from another place, say the unit testing script, then
# the main function will not run.
main() unless caller;
