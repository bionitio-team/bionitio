'''
Unit tests for biotool.

Usage: python -m unittest -v biotool_test
'''

import unittest
from StringIO import StringIO
from Bio import SeqIO
from biotool import FastaStats

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


if __name__ == '__main__':
    unittest.main()
