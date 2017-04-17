#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use File::Spec;
use Bio::SeqIO;
use Log::Log4perl qw(get_logger :nowarn);

my $EXIT_SUCCESS = 0;
my $EXIT_FILE_IO_ERROR = 1;
my $EXIT_COMMAND_LINE_ERROR = 2;
my $EXIT_FASTA_FILE_ERROR = 3;
my @COLUMNS = qw(FILENAME TOTAL NUMSEQ MIN AVG MAX);
my ( undef, undef, $PROGRAM_NAME ) = File::Spec->splitpath($0);
my $VERSION        = "1.0";
my $DEFAULT_MINLEN = 0;
my $verbose        = 0;
my $logger;

# Keep a copy of ARGV, so we can write the original to the log file
my @ORIGINAL_ARGV = @ARGV;

sub get_options {
    my $minlen  = 0;
    my $logfile = "";

    GetOptions(
        "help"    => sub { usage($EXIT_SUCCESS) },
        "version" => sub { print "$PROGRAM_NAME $VERSION\n"; exit(0); },
        "verbose" => sub { $verbose++ },
        "minlen=i" => \$minlen,
        "log=s"    => \$logfile,
    ) or usage($EXIT_COMMAND_LINE_ERROR);

    # XXX this does not look portable!
    push @ARGV, "/dev/stdin" unless @ARGV;

    return ( minlen => $minlen, logfile => $logfile );
}

sub init_logging {

    # message pattern is:
    #   %d date (and time), %p priority (level), %m message, %n newline
    # minimum priority is INFO

    my ($logfile) = @_;

    if ( $logfile ne "" ) {
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
    $logger = get_logger("Biotool");

    $logger->info("program started");

    # Log the program name and command line arguments
    $logger->info("command line arguments: $0 @ORIGINAL_ARGV");
}

# Print an error message to stderr, prefixed by the program name and 'ERROR'.
# Then exit program with supplied exit status.
#
# Arguments:
#     message: an error message as a string.
#     exit_status: a positive integer representing the exit status of the
#         program.
sub exit_with_error {
    my ( $message, $exit_status) = @_;
    $logger->error($message); 
    print STDERR "$PROGRAM_NAME ERROR: $message\n";
    exit($exit_status);
}

sub main {
    my %options = get_options();
    init_logging( $options{logfile} );

    my $badfiles = 0;

    print tsv( \@COLUMNS );

    for my $file (@ARGV) {
        $logger->info("Processing FASTA file from $file");
        unless ( -r $file ) {
	    exit_with_error("Unable to read file: $file", $EXIT_FILE_IO_ERROR);
        }
        my $res = process_file( $file, $options{minlen} );
        print tsv($res) if $res;
    }
    exit($EXIT_SUCCESS);
}

sub process_file {
    my ( $fname, $minlen ) = @_;

    # to collect stats
    my $bp  = 0;
    my $n   = 0;
    my $min = 1E12;
    my $max = 0;

    # loop over each sequence
    my $in = Bio::SeqIO->new( -file => $fname, -format => 'fasta' );
    while ( my $seq = $in->next_seq ) {
        my $L = $seq->length;
        next if $L < $minlen;
        print STDERR tsv( [ $fname, $seq->id, $seq->length ] ) if $verbose >= 2;
        $n++;
        $bp += $seq->length;
        $min = $L if $L < $min;
        $max = $L if $L > $max;
    }

    # FILENAME TOTAL NUMSEQ MIN AVG MAX
    return $n <= 0
      ? [ $fname, $n, $bp, '-', '-', '-' ]
      : [ $fname, $n, $bp, $min, int( $bp / $n ), $max ];
}

sub tsv {
    my ( $row, $sep ) = @_;
    $sep ||= "\t";
    return join( $sep, @$row ) . "\n";
}

sub usage {
    my ($exit_status) = @_;
    print <<"EOF";
Synopsis:
  Print fasta stats
Usage:
  $PROGRAM_NAME [options] contigs.fasta [another.fa ...]
Options:
  --help       Show this help
  --version    Print version and exit
  --verbose    Print more stuff about what's happening
  --minlen N   Minimum length sequence to include in stats (default=$DEFAULT_MINLEN)
  --log FILE   Log messages are written to FILE
EOF
    exit $exit_status;
}

main();
