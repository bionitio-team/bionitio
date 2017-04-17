#!/usr/bin/env perl

use strict;
use warnings 'FATAL' => 'all';
use Getopt::Long;
use File::Spec;
use Bio::SeqIO;
use Log::Log4perl qw(get_logger :nowarn);

my $EXIT_SUCCESS = 0;
my $EXIT_FILE_IO_ERROR = 1;
my $EXIT_COMMAND_LINE_ERROR = 2;
my $EXIT_FASTA_FILE_ERROR = 3;
my @COLUMNS = qw(FILENAME TOTAL NUMSEQ MIN AVG MAX);
my (undef, undef, $PROGRAM_NAME) = File::Spec->splitpath($0);
my $VERSION        = "1.0";
my $DEFAULT_MINLEN = 0;
my $logger;

sub get_options {
    my $minlen  = 0;
    my $logfile = "";

    GetOptions(
        "help"    => sub {usage($EXIT_SUCCESS)},
        "version" => sub {print "$PROGRAM_NAME $VERSION\n"; exit($EXIT_SUCCESS);},
        "minlen=i" => \$minlen,
        "log=s"    => \$logfile,
    ) or usage($EXIT_COMMAND_LINE_ERROR);

    return (minlen => $minlen, logfile => $logfile);
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
        Log::Log4perl->init(\$log_conf);
    }

    # Obtain a logger instance
    $logger = get_logger("Biotool");
    $logger->info("program started");
    # Log the program name and command line arguments
    $logger->info("command line arguments: $0 @ARGV");
}

# Print an error message to stderr, prefixed by the program name and 'ERROR'.
# Then exit program with supplied exit status.
#
# Arguments:
#     message: an error message as a string.
#     exit_status: a positive integer representing the exit status of the
#         program.
sub exit_with_error {
    my ($message, $exit_status) = @_;
    $logger->error($message); 
    print STDERR "$PROGRAM_NAME ERROR: $message\n";
    exit($exit_status);
}

sub process_file {
    my ($filename, $filehandle, $minlen_threshold) = @_;

    # to collect stats
    my $num_bases  = 0;
    my $num_seqs   = 0;
    my $min_len = undef;
    my $max_len = undef;

    # XXX should catch exceptions raised by SeqIO, no idea how to do it
    my $in = Bio::SeqIO->new(-fh => $filehandle, -format => 'fasta');
    while (my $seq = $in->next_seq) {
        my $this_len = $seq->length;
        # Skip this sequence if its length is less than the threshold
        if ($this_len >= $minlen_threshold) {
            $num_seqs++;
            $num_bases += $this_len;
            $min_len = $this_len if (!defined($min_len)) || $this_len < $min_len;
            $max_len = $this_len if (!defined($max_len)) || $this_len > $max_len;
        }
    }

    # FILENAME TOTAL NUMSEQ MIN AVG MAX
    return $num_seqs <= 0
      ? [$filename, $num_seqs, $num_bases, '-', '-', '-']
      : [$filename, $num_seqs, $num_bases, $min_len, int($num_bases / $num_seqs), $max_len];
}

sub tsv {
    my ($row, $sep) = @_;
    $sep ||= "\t";
    return join($sep, @$row) . "\n";
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
  --minlen N   Minimum length sequence to include in stats (default=$DEFAULT_MINLEN)
  --log FILE   Log messages are written to FILE
EOF
    exit $exit_status;
}

sub process_files {
    my (%options) = @_;

    print tsv(\@COLUMNS);

    if (@ARGV) {
        for my $filename (@ARGV) {
            $logger->info("Processing FASTA file from $filename");
            if (open(my $filehandle, $filename)) {
                my $res = process_file($filename, $filehandle, $options{minlen});
                print tsv($res) if $res;
            }
            else {
                exit_with_error("Could not open $filename for reading", $EXIT_FILE_IO_ERROR);
            }
        }
    }
    else {
        $logger->info("Processing FASTA file from stdin");
        my $res = process_file("stdin", \*STDIN, $options{minlen});
        print tsv($res) if $res;
    }
}

sub main {
    my %options = get_options();
    init_logging($options{logfile});
    process_files(%options);
    exit($EXIT_SUCCESS);
}

main();
