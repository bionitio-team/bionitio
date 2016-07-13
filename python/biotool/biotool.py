'''
Biotool.

The program reads one or more input FASTA files. For each file it computes a
variety of statistics, and then prints a summary of the statistics as output.
'''

from __future__ import print_function
from argparse import ArgumentParser
from math import floor
import sys
import unittest
from StringIO import StringIO
from Bio import SeqIO
import pkg_resources


EXIT_FILE_IO_ERROR = 1
EXIT_COMMAND_LINE_ERROR = 2
EXIT_FASTA_FILE_ERROR = 3
DEFAULT_MIN_LEN = 0
DEFAULT_VERBOSE = False
HEADER = 'FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX'
PROGRAM_NAME = "biotool-py"


try:
    PROGRAM_VERSION = pkg_resources.require(PROGRAM_NAME)[0].version
except pkg_resources.DistributionNotFound:
    PROGRAM_VERSION = "undefined_version"


def exit_with_error(message, exit_status):
    '''Print an error message to stderr, prefixed by the program name and 'ERROR'.
    Then exit program with supplied exit status.

    Arguments:
        message: an error message as a string.
        exit_status: a positive integer representing the exit status of the
            program.
    '''
    print("{} ERROR: {}, exiting".format(PROGRAM_NAME, message), file=sys.stderr)
    sys.exit(exit_status)


def parse_args():
    '''Parse command line arguments.
    Returns Options object with command line argument values as attributes.
    Will exit the program on a command line error.
    '''
    parser = ArgumentParser(description='Print fasta stats')
    parser.add_argument(
        '--minlen',
        metavar='N',
        type=int,
        default=DEFAULT_MIN_LEN,
        help='Minimum length sequence to include in stats (default {})'.format(
            DEFAULT_MIN_LEN))
    parser.add_argument('--version',
                        action='version',
                        version='%(prog)s ' + PROGRAM_VERSION)
    parser.add_argument('--verbose',
                        action='store_true',
                        default=DEFAULT_VERBOSE,
                        help="Print more stuff about what's happening")
    parser.add_argument('fasta_files',
                        nargs='*',
                        metavar='FASTA_FILE',
                        type=str,
                        help='Input FASTA files')
    return parser.parse_args()


class FastaStats(object):
    '''Compute various statistics for a FASTA file:

    num_seqs: the number of sequences in the file satisfying the minimum
       length requirement (minlen_threshold).
    num_bases: the total length of all the counted sequences.
    min_len: the minimum length of the counted sequences.
    max_len: the maximum length of the counted sequences.
    average: the average length of the counted sequences rounded down
       to an integer.
    '''
    #pylint: disable=too-many-arguments
    def __init__(self,
                 num_seqs=None,
                 num_bases=None,
                 min_len=None,
                 max_len=None,
                 average=None):
        "Build an empty FastaStats object"
        self.num_seqs = num_seqs
        self.num_bases = num_bases
        self.min_len = min_len
        self.max_len = max_len
        self.average = average

    def __eq__(self, other):
        "Two FastaStats objects are equal iff their attributes are equal"
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        else:
            return False

    def __repr__(self):
        "Generate a printable representation of a FastaStats object"
        return "FastaStats(num_seqs={}, num_bases={}, min_len={}, max_len={}, " \
            "average={})".format(
                self.num_seqs, self.num_bases, self.min_len, self.max_len,
                self.average)

    def from_file(self, fasta_file, minlen_threshold=DEFAULT_MIN_LEN):
        '''Compute a FastaStats object from an input FASTA file.

        Arguments:
           fasta_file: an open file object for the FASTA file
           minlen_threshold: the minimum length sequence to consider in
              computing the statistics. Sequences in the input FASTA file
              which have a length less than this value are ignored and not
              considered in the resulting statistics.
        Result:
           A FastaStats object
        '''
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
        '''Generate a pretty printable representation of a FastaStats object
        suitable for output of the program. The output is a tab-delimited
        string containing the filename of the input FASTA file followed by
        the attributes of the object. If 0 sequences were read from the FASTA
        file then num_seqs and num_bases are output as 0, and min_len, average
        and max_len are output as a dash "-".

        Arguments:
           filename: the name of the input FASTA file
        Result:
           A string suitable for pretty printed output
        '''
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
    '''Compute and print FastaStats for each input FASTA file specified on the
    command line. If no FASTA files are specified on the command line then
    read from the standard input (stdin).

    Arguments:
       options: the command line options of the program
    Result:
       None
    '''
    if options.fasta_files:
        for fasta_filename in options.fasta_files:
            try:
                fasta_file = open(fasta_filename)
            except IOError as exception:
                exit_with_error(str(exception), EXIT_FILE_IO_ERROR)
            else:
                with fasta_file:
                    stats = FastaStats().from_file(fasta_file, options.minlen)
                    print(stats.pretty(fasta_filename))
    else:
        stats = FastaStats().from_file(sys.stdin, options.minlen)
        print(stats.pretty("stdin"))


def main():
    "Orchestrate the execution of the program"
    options = parse_args()
    print(HEADER)
    process_files(options)


# If this script is run from the command line then call the main function.
if __name__ == '__main__':
    main()

# Unit tests follow

class TestFastaStats(unittest.TestCase):
    '''Unit tests for FastaStats'''
    def do_test(self, input_str, minlen, expected):
        "Wrapper function for testing FastaStats"
        result = FastaStats().from_file(StringIO(input_str), minlen)
        self.assertEqual(expected, result)

    def test_zero_byte_input(self):
        "Test input containing zero bytes"
        expected = FastaStats(num_seqs=0,
                              num_bases=0,
                              min_len=None,
                              max_len=None,
                              average=None)
        self.do_test('', 0, expected)

    def test_single_newline_input(self):
        "Test input containing a newline (\n) character"
        expected = FastaStats(num_seqs=0,
                              num_bases=0,
                              min_len=None,
                              max_len=None,
                              average=None)
        self.do_test('\n', 0, expected)

    def test_single_greater_than_input(self):
        "Test input containing a single greater-than (>) character"
        expected = FastaStats(num_seqs=1,
                              num_bases=0,
                              min_len=0,
                              max_len=0,
                              average=0)
        self.do_test('>', 0, expected)

    def test_one_sequence(self):
        "Test input containing one sequence"
        expected = FastaStats(num_seqs=1,
                              num_bases=5,
                              min_len=5,
                              max_len=5,
                              average=5)
        self.do_test(">header\nATGC\nA", 0, expected)

    def test_two_sequences(self):
        "Test input containing two sequences"
        expected = FastaStats(num_seqs=2,
                              num_bases=9,
                              min_len=2,
                              max_len=7,
                              average=4)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 0, expected)

    def test_no_header(self):
        "Test input containing sequence without preceding header"
        expected = FastaStats(num_seqs=0,
                              num_bases=0,
                              min_len=None,
                              max_len=None,
                              average=None)
        self.do_test("no header\n", 0, expected)

    def test_minlen_less_than_all(self):
        "Test input when --minlen is less than 2 out of 2 sequences"
        expected = FastaStats(num_seqs=2,
                              num_bases=9,
                              min_len=2,
                              max_len=7,
                              average=4)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 2, expected)

    def test_minlen_greater_than_one(self):
        "Test input when --minlen is less than 1 out of 2 sequences"
        expected = FastaStats(num_seqs=1,
                              num_bases=7,
                              min_len=7,
                              max_len=7,
                              average=7)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 3, expected)

    def test_minlen_greater_than_all(self):
        "Test input when --minlen is greater than 2 out of 2 sequences"
        expected = FastaStats(num_seqs=0,
                              num_bases=0,
                              min_len=None,
                              max_len=None,
                              average=None)
        self.do_test(">header1\nATGC\nAGG\n>header2\nTT\n", 8, expected)
