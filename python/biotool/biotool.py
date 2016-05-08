from argparse import ArgumentParser
from version import program_version 
from Bio import SeqIO
from math import floor
from collections import namedtuple
from sys import stdin


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
        'fasta_files', nargs='*', metavar='FASTA_FILE', type=str,
        help='Input FASTA files')
    return parser.parse_args()


class FastaStats(object):
    def __init__(self, fasta_file, minlen):
        if fasta_file is stdin:
            self.filename = "stdin"
        else:
            self.filename = fasta_file 

        num_seqs = num_bases = 0
        min_len = max_len = None
        for seq in SeqIO.parse(fasta_file, "fasta"):
            this_len = len(seq)
            if this_len >= minlen:
                if num_seqs == 0:
                    min_len = max_len = this_len
                else:
                    min_len = min(this_len, min_len)
                    max_len = max(this_len, max_len) 
                num_seqs += 1
                num_bases += this_len 

        self.num_seqs = num_seqs
        self.num_bases = num_bases
        self.min_len = min_len
        self.max_len = max_len
        if num_seqs > 0:
            self.average = int(floor(float(num_bases) / num_seqs))
        else:
            self.average = None


    def __str__(self):
        if self.num_seqs > 0:
            num_seqs = str(self.num_seqs)
            num_bases = str(self.num_bases)
            min_len = str(self.min_len)
            average = str(self.average)
            max_len = str(self.max_len)
        else:
            num_seqs = "0"
            num_bases = "0"
            min_len = average = max_len = "-"
        return "\t".join([self.filename, num_seqs, num_bases, min_len, average,
           max_len])


def process_files(options):
    print(HEADER)
    if options.fasta_files:
        files = options.fasta_files
    else:
        files = [stdin]
    for fasta_file in files:
        print(FastaStats(fasta_file, options.minlen))


def main():
    process_files(parseArgs())


if __name__ == '__main__':
    main()
