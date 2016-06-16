from argparse import ArgumentParser
from version import program_version 
from Bio import SeqIO
from math import floor
from sys import stdin
import unittest
from StringIO import StringIO


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
    def __init__(self, num_seqs=None, num_bases=None, min_len=None, max_len=None, average=None):
        self.num_seqs = num_seqs
        self.num_bases = num_bases
        self.min_len = min_len
        self.max_len = max_len
        self.average = average


    def __eq__(self, other):
        if type(other) is type(self): 
            return self.__dict__ == other.__dict__
        else:
            return False

    def __repr__(self):
        return "FastaStats(num_seqs={}, num_bases={}, min_len={}, max_len={}, average={})".format(
            self.num_seqs, self.num_bases, self.min_len, self.max_len, self.average)


    def from_file(self, fasta_file, minlen_threshold=DEFAULT_MIN_LEN):
        num_seqs = num_bases = 0
        min_len = max_len = None
        for seq in SeqIO.parse(fasta_file, "fasta"):
            this_len = len(seq)
            if this_len >= minlen_threshold:
                if num_seqs == 0:
                    min_len = max_len = this_len
                else:
                    min_len = min(this_len, min_len)
                    max_len = max(this_len, max_len) 
                num_seqs += 1
                num_bases += this_len 
        if num_seqs > 0:
            self.average = int(floor(float(num_bases) / num_seqs))
        else:
            self.average = None
        self.num_seqs = num_seqs
        self.num_bases = num_bases
        self.min_len = min_len
        self.max_len = max_len
        return self


    def pretty(self, filename):
        if self.num_seqs > 0:
            num_seqs = str(self.num_seqs)
            num_bases = str(self.num_bases)
            min_len = str(self.min_len)
            average = str(self.average)
            max_len = str(self.max_len)
        else:
            num_seqs = num_bases = "0"
            min_len = average = max_len = "-"
        return "\t".join([filename, num_seqs, num_bases, min_len, average,
           max_len])


def process_files(options):
    print(HEADER)
    if options.fasta_files:
        for fasta_filename in options.fasta_files:
            # XXX catch file IO issues here and handle gracefully
            with open(fasta_filename) as fasta_file:
               stats = FastaStats().from_file(fasta_file, options.minlen)
               print(stats.pretty(fasta_filename))
    else:
        stats = FastaStats().from_file(stdin, options.minlen)
        print(stats.pretty("stdin"))


def main():
    process_files(parseArgs())


if __name__ == '__main__':
    main()


class TestFastaStats(unittest.TestCase):
    def do_test(self, input, minlen, expected):
        result = FastaStats().from_file(StringIO(input), minlen)
        self.assertEqual(expected, result)

    def test_zero_byte_input(self):
        expected = FastaStats(num_seqs=0, num_bases=0, min_len=None, max_len=None, average=None)
        self.do_test('', 0, expected)

    def test_single_newline_input(self):
        expected = FastaStats(num_seqs=0, num_bases=0, min_len=None, max_len=None, average=None)
        self.do_test('\n', 0, expected) 

    def test_single_greater_than_input(self):
        expected = FastaStats(num_seqs=1, num_bases=0, min_len=0, max_len=0, average=0)
        self.do_test('>', 0, expected) 

    def test_one_sequence(self):
        expected = FastaStats(num_seqs=1, num_bases=5, min_len=5, max_len=5, average=5)
        self.do_test(">header\nATGC\nA", 0, expected) 

    def test_two_sequences(self):
        expected = FastaStats(num_seqs=2, num_bases=9, min_len=2, max_len=7, average=4)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 0, expected) 

    def test_no_header(self):
        expected = FastaStats(num_seqs=0, num_bases=0, min_len=None, max_len=None, average=None)
        self.do_test("no header\n", 0, expected) 

    def test_minlen_less_than_all(self):
        expected = FastaStats(num_seqs=2, num_bases=9, min_len=2, max_len=7, average=4)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 2, expected) 

    def test_minlen_greater_than_one(self):
        expected = FastaStats(num_seqs=1, num_bases=7, min_len=7, max_len=7, average=7)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 3, expected) 

    def test_minlen_greater_than_all(self):
        expected = FastaStats(num_seqs=0, num_bases=0, min_len=None, max_len=None, average=None)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 8, expected) 
