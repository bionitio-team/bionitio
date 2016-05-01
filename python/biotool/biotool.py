#!/usr/bin/env python

from argparse import ArgumentParser
from version import program_version 
from Bio import SeqIO


DEFAULT_MIN_LEN = 0
DEFAULT_VERBOSE = False
HEADER = 'FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX' 


def parseArgs():
    parser = ArgumentParser(description='Print fasta stats')
    parser.add_argument(
        '--minlen', metavar='N', type=int, default=DEFAULT_MIN_LEN,
        help='Minimum length sequence to include in stats (default {})'.format(DEFAULT_MIN_LEN))
    parser.add_argument('--version', action='version', version='%(prog)s ' + program_version)
    parser.add_argument(
        '--verbose', action='store_true', default=DEFAULT_VERBOSE,
        help="Print more stuff about what's happening")
    parser.add_argument(
        'fasta_files', nargs='+', metavar='FASTA_FILE', type=str,
        help='Input FASTA files')
    return parser.parse_args()


def process_files(options):
    for filepath in options.fasta_files:
        min_len = max_len = None
        num_sequences = num_bases = 0
        for sequence in SeqIO.parse(filepath, "fasta"):
            num_sequences += 1
            this_len = len(sequence)
            num_bases += this_len 
            if min_len is None or this_len < min_len:
                min_len = this_len
            if max_len is None or this_len > max_len:
                max_len = this_len
        if num_sequences > 0:
            average = int(round(float(num_bases) / num_sequences))
            print("{}\t{}\t{}\t{}\t{}\t{}".format(filepath, num_sequences, num_bases,
                min_len, average, max_len))
        else:
            print("{}\t{}\t{}\t-\t-\t-".format(filepath, num_sequences, num_bases))

 
def main():
    options = parseArgs()
    print(HEADER)
    process_files(options)


if __name__ == '__main__':
    main()
