#!/usr/bin/env bash

#............................................................................
# Defaults

set -e

VERSION="1.0"
EXE=$(basename $0)
MINLEN=0
VERBOSE=0
#TEMPDIR=$(mktemp --tmpdir -d "${EXE}-tmp-XXXXXX")
# make sure the above temp folder is deleted at exit or error
#trap "rm -fr '$TEMPDIR'" exit

#............................................................................
# Reusable functions

function msg {
  echo -e "$*" 1>&2
}

function err {
  echo "ERROR: $*" 1>&2
  exit 1
}

function exitOnFailure {
  if [ $1 -ne 0 ]; then
    err "$2 failed to run (errorcode=$1)"
  fi
}

function usage {
  msg "Usage:\n  $EXE [options] <file.fa> [more.fa ...]"
  msg "Options:"
  msg "  -h          Show this help"
  msg "  -V          Print version and exit";
  msg "  -v          Verbosity level ($VERBOSE)"
  msg "  -m          Min seq length to consider ($MINLEN)"
  exit 1 
}

#...........................................................................
# Parse the command line options

while getopts 'hVvm:' opt ; do
  case $opt in
    h)
      usage
      ;;
    V)
      msg "$EXE $VERSION"
      exit
      ;;    
    v)
      VERBOSE=1
      ;;    
    m)
      MINLEN=$OPTARG
      ;;    
  esac
done

# skip over out options
shift $((OPTIND-1))

# read our mandatory positional parameters

if [ $# -lt 1 ]; then
  usage
fi

#...........................................................................
# Parse the command line options

echo -e "FILE\tNSEQ\tTOTAL\tMIN\tAVG\tMAX"
for FASTA in $@ ; do
  msg "Need to process $FASTA here"
  NSEQ=0
  BP=0
  MIN=0
  AVG=0
  MAX=999999999
  for SEQ in $(awk 'BEGIN{RS=">"} NR>1 { sub("\n","\t"); gsub("\n",""); print $0 }' $FASTA | cut -f2); do
    L=${#SEQ}
    let "BP += L"
    let "NSEQ += 1"
  done
  let "AVG = BP / NSEQ"
  echo -e "$FASTA\t$NSEQ\t$BP\t$MIN\t$AVG\t$MAX"
  # should probably just use datamash !
done







