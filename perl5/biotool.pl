#!/usr/bin/env perl

#.........................................................................
# modules

use strict;
use warnings;
use Getopt::Long;
use File::Spec;
use Bio::SeqIO;

#.........................................................................
# globals

my @COLUMNS = qw(FILENAME TOTAL NUMSEQ MIN AVG MAX);
my(undef,undef,$EXE) = File::Spec->splitpath($0);
my $VERSION = "1.0";
my $verbose = 0;
my $minlen = 0;

#.........................................................................
# process command line 

GetOptions(
  "help"     => sub { usage(0) },
  "version"  => sub { print "$EXE $VERSION\n"; exit; },
  "verbose"  => sub { $verbose++ },
  "minlen=i" => \$minlen,
)
or usage(1);

push @ARGV, "/dev/stdin" unless @ARGV;

#.........................................................................
# MAIN

print tsv(\@COLUMNS);

for my $file (@ARGV) {
  print STDERR "Processing: $file\n" if $verbose >= 1;
  my $res = process_file($file);
  if ($res) {
    print tsv($res);
  }
  else {
    print STDERR "Skipping $file - doesn't seem to be FASTA?\n";
  }
}

#.........................................................................

sub process_file {
  my($fname) = @_;
  
  # to collect stats
  my $bp=0;
  my $n=0;
  my $min=1E9;
  my $max=0;

  # loop over each sequence
  my $in = Bio::SeqIO->new(-file=>$fname, -format=>'fasta');
  while (my $seq = $in->next_seq) {
    my $L = $seq->length;
    next if $L < $minlen;
    print STDERR tsv( [ $fname, $seq->id, $seq->length ] ) if $verbose >= 2;
    $n++;
    $bp += $seq->length;
    $min = $L if $L < $min;
    $max = $L if $L > $max;
  }
  
  return if $n <= 0;
  # FILENAME TOTAL NUMSEQ MIN AVG MAX
  return [ $fname, $n, $bp, $min, int($bp/$n), $max ];
}

#.........................................................................

sub tsv {
  my($row, $sep) = @_;
  $sep ||= "\t";
  return join($sep, @$row)."\n";
}

#.........................................................................
# Usage info to stdout

sub usage {
  my($errcode) = 0;
  print <<"EOF";
Synopsis:
  Print fasta stats
Usage:
  $EXE [options] contigs.fasta [another.fa ...]
Options:
  --help       Show this help
  --version    Print version and exit
  --verbose    Print more stuff about what's happening
  --minlen N   Minimum length sequence to include in stats (default=$minlen)
EOF
  exit $errcode;
}
