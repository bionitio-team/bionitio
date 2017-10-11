# Overview

This is an R implementation of bionitio.

The program reads one or more input FASTA files. For each file it computes a variety of statistics, and then prints a summary of the statistics as output.

The goal is to provide a solid foundation for new bioinformatics command line tools, and is an ideal starting place for new projects.

# Licence

This program is released as open source software under the terms of [MIT License](https://raw.githubusercontent.com/bionitio-paper/bionitio/master/LICENSE)

# Installing

### Installing R

Download R from [CRAN](https://www.r-project.org/) and follow the instructions
there to install it.

bionitio.R was tested on R version 3.2.

### Installing dependencies

Bionitio requires the optparse and seqinr packages which can be installed by:

```bash
Rscript -e "install.packages(c('optparse', 'seqinr'), repos='http://cran.rstudio.org')"
```

### Installing the bionitio package

To install the bionitio package:

```bash
R CMD INSTALL bionitio
```

# General behaviour

Bionitio accepts zero or more FASTA filenames on the command line. If zero filenames are specified it reads a single FASTA file from the standard input device (stdin). Otherwise it reads each named FASTA file in the order specified on the command line. Bionitio reads each input FASTA file, computes various statistics about the contents of the file, and then displays a tab-delimited summary of the statistics as output. Each input file produces at most one output line of statistics. Each line of output is prefixed by the input filename or by the text "`stdin`" if the standard input device was used.

An optional command line argument `--minlen` can be supplied. Sequences with length strictly less than the given value will be ignored by bionitio and do not contribute to the computed statistics. By default `--minlen` is set to zero.

These are the statistics computed by bionitio, for all sequences with length greater-than-or-equal-to `--minlen`:

* *NUMSEQ*: the number of sequences in the file satisfying the minimum length requirement.
* *TOTAL*: the total length of all the counted sequences.
* *MIN*: the minimum length of the counted sequences.
* *AVERAGE*: the average length of the counted sequences rounded down to an integer.
* *MAX*: the maximum length of the counted sequences.

If there are zero sequences counted in a file, the values of MIN, AVERAGE and MAX cannot be computed. In that case bionitio will print a dash (`-`) in the place of the numerical value. Note that when `--minlen` is set to a value greater than zero it is possible that an input FASTA file does not contain any sequences with length greater-than-or-equal-to the specified value. If this situation arises bionitio acts in the same way as if there are no sequences in the file.

# Usage

In the examples below, `%` indicates the command line prompt.

## Help message

Bionitio can display usage information on the command line via the `-h` or `--help` argument:

```
% ./bionitio.R -h
Usage: ./bionitio.R [OPTIONS] [FASTA_FILE [FASTA_FILE ...]]
Print FASTA stats.

Positional arguments:
	FASTA_FILE: Input FASTA files. Use - to read from stdin.


Options:
	--minlen=MINLEN
		Minimum length sequence to include in stats [default: 0]

	--verbose
		Print more stuff about what's happening

	--version
		Print version and exit

	-h, --help
		Show this help message and exit
```

## Reading FASTA files named on the command line

Bionitio accepts zero or more named FASTA files on the command line. These must be specified following all other command line arguments. If zero files are named, bionitio will read a single FASTA file from the standard input device (stdin).

There are no restrictions on the name of the FASTA files. Often FASTA filenames end in `.fa` or `.fasta`, but that is merely a convention, which is not enforced by bionitio. 

The example below illustrates bionitio applied to a single named FASTA file called `file1.fa`:
```
% ./bionitio.R file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
```

The example below illustrates bionitio applied to three named FASTA files called `file1.fa`, `file2.fa` and `file3.fa`:
```
% ./bionitio.R file1.fa file2.fa file3.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
file2.fa	5264	3801855	31	722	53540
file3.fa	5264	3801855	31	722	53540
```

## Reading a single FASTA file from standard input 

The example below illustrates bionitio reading a FASTA file from standard input. In this example we have redirected the contents of a file called `file1.fa` into the standard input using the shell redirection operator `<`:

```
% ./bionitio.R < file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

Equivalently, you could achieve the same result by piping a FASTA file into bionitio:

```
% cat file1.fa | ./bionitio.R
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

## Filtering sequences by length 

Bionitio provides an optional command line argument `--minlen` which causes it to ignore (not count) any sequences in the input FASTA files with length strictly less than the supplied value. 

The example below illustrates bionitio applied to a single FASTA file called `file`.fa` with a `--minlen` filter of `1000`.
```
% ./bionitio.R --minlen 1000 file.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	4711	2801855	1021	929	53540
```

## Empty files

It is possible that the input FASTA file contains zero sequences, or, when the `--minlen` command line argument is used, it is possible that the file contains no sequences of length greater-than-or-equal-to the supplied value. In both of those cases bionitio will not be able to compute minimum, maximum or average sequence lengths, and instead it shows output in the following way:

The example below illustrates bionitio applied to a single FASTA file called `empty`.fa` which contains zero sequences:
```
% ./bionitio.R empty.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
empty.fa	0	0	-	-	-
```

# Exit status values

Bionitio returns the following exit status values:

* **0**: The program completed successfully.
* **1**: File I/O error. This can occur if at least one of the input FASTA files cannot be opened for reading. This can occur because the file does not exist at the specified path, or bionitio does not have permission to read from the file. 
* **2**: A command line error occurred. This can happen if the user specifies an incorrect command line argument.
* **3**: Input FASTA file is invalid. This can occur if bionitio can read an input file but the file format is invalid. 

# Testing

### Dependencies for testing

For testing, bionitio requires the devtools and lintr packages which can be installed by:

```bash
Rscript -e "install.packages(c('devtools', 'lintr'), repos='http://cran.rstudio.org')"
```

### Running tests

Unit tests and linting can be run by:

```bash
Rscript -e "devtools::test('bionitio')"
```

Or to check the whole package:

```bash
Rscript -e "devtools::check('bionitio')"
```

### Test input files

A set of sample test input files is provided in the `test_data` folder.

```
% ./bionitio.R ../test_data/two_sequence.fasta 
FILENAME	TOTAL	NUMSEQ	MIN	AVG	MAX
../test_data/two_sequence.fasta	2	357	120	178	237
```

# Bugs

File at our [Issue Tracker](https://github.com/bionitio-paper/bionitio/issues)

