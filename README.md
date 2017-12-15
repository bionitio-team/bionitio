[![travis](https://travis-ci.org/bionitio-team/bionitio.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio)

# Overview 

This project provides a template for implementing command line bioinformatics tools in various programming languages, 
demonstrating best practice using a toy example called `bionitio`.

## Languages


| Language | Travis Testing Status |
|----------|-----------------------|
| C        | [![travis](https://travis-ci.org/bionitio-team/bionitio-c.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-c) || 


* C++
* C#
* Clojure
* Java
* Javascript
* Haskell
* Perl5
* Python
* R
* Ruby
* Rustk

The program reads one or more input FASTA files. For each file it computes a variety of simple statistics, and then prints a summary output.

The goal is to provide a solid foundation for new bioinformatics command line tools, and is an ideal starting place for new projects. An additional advantage of bionitio is that it allows us to compare programming styles in different languages.

Basic features of the tool include:

* Command line argument parsing.
* Reading input from files or optionally from standard input.
* The use of library code for parsing a common bioinformatics file format (FASTA).
* Optional logging.
* Defined exit status values.
* A test suite. 
* A version number.
* (Where possible) standardised software packaging using programming language specific mechanisms.
* A standard open-source software license. 
* User documentation.
* Code documentation.

Where possible we have tried to follow the recommended conventions for programming style for each implementation language.

# License

The bionitio project is released as open source software under the terms of [MIT License](https://raw.githubusercontent.com/bionitio-team/bionitio/master/LICENSE).
However, we grant permission to users who derive their own projects from bionitio to apply their own license to their derived works. Licenses applied to projects deriving from bionitio do not affect in any way the license of the overall bionitio project, or licenses applied to other independent derivations.

# Starting a new project from bionitio

One of the main goals of bionitio is to provide a good place to start writing bioinformatics command line tools. To make that easy we've provided a shell script called `bionitio-boot.sh` to help you start a new project, which is run like so:

```
boot/bionitio-boot.sh -l rust -n skynet -c BSD-3-Clause
```

The example above starts a fresh project called `skynet` using Rust as the implementation language. A new git repository will be created in a sub-directory called `skynet`, and the project using the BSD 3 Clause license.

When setting up a new project using `bionitio-boot.sh` You must specify the following things: 

Required:

* -l LANGUAGE: the programming language you want to use (one of: c, clojure, cpp, haskell, java, js, perl5, python, r, ruby, rust)
* -n NAME: the name of your new project.

Optional:

* -c LICENSE: the license that you want to assign to your new project (one of: Apache-2.0, BSD-2-Clause, BSD-3-Clause, GPL-2.0, GPL-3.0, MIT). If you do not specify a license then it defaults to the MIT license.

If you don't have a local copy of the script, you can run it from the web like so, using curl:

```
curl -sSf https://raw.githubusercontent.com/bionitio-team/bionitio/master/boot/bionitio-boot.sh \
 | bash -s -- -l rust -n skynet -c BSD-3-Clause
```

If you prefer not to run a shell script from the web, then you can either clone the whole bionitio repository, or just make a local copy of the `bionitio-boot.sh` script, and run it locally, as shown below:

```
# Copy the script to your local computer
curl https://raw.githubusercontent.com/bionitio-team/bionitio/master/boot/bionitio-boot.sh > bionitio-boot.sh
# Run the script on your local computer
bash bionitio-boot.sh -l rust -n skynet -c BSD-3-Clause
```

# General behaviour

Bionitio accepts zero or more FASTA filenames on the command line. It processes each file in sequence, computing various simple statistics about the reads contained in the file,
and then displays a tab-delimited summary of the statistics as output. If zero filenames are specified it reads a single FASTA file from the standard input device (stdin). 

An optional command line argument `--minlen` can be supplied. Sequences with length strictly less than the given value will be ignored by bionitio and do not contribute to the computed statistics. By default `--minlen` is set to zero.

These are the statistics computed by bionitio, for all sequences with length greater-than-or-equal-to `--minlen`:

* NUMSEQ: the number of sequences in the file satisfying the minimum length requirement.
* TOTAL: the total length of all the counted sequences.
* MIN: the minimum length of the counted sequences.
* AVERAGE: the average length of the counted sequences rounded down to an integer.
* MAX: the maximum length of the counted sequences.

If there are zero sequences counted in a file, the values of MIN, AVERAGE and MAX cannot be computed. In that situation bionitio will print a dash (`-`) in the place of the numerical values. Note that when `--minlen` is set to a value greater than zero it is possible that an input FASTA file does not contain any sequences with length greater-than-or-equal-to the specified value. If this situation arises bionitio acts in the same way as if there are no sequences in the file.

Bionitio processes each FASTA file one sequence at a time. Therefore the memory usage is proportional to the longest sequence in the file.

# Installation and usage 

Installation and usage instructions are provided separately for each of the implementations of bionitio. Please see the README.md files in the corresponding sub-folders for each implementation.

Each implementation of bionitio conforms to a standard command line interface, illustrated below. There may be small cosmetic differences in help messages due to programming language idiosyncrasies, but otherwise the behaviour of all implementations should be the same. The name of the executable program for each version is different, for example, the Python version is called `bionitio-py`. The examples below show the output from the Python version, however the other implementations will behave similarly.

In the examples below, `$` indicates the command line prompt.

## Help message

Bionitio can display usage information on the command line via the `-h` or `--help` argument:
```
$ bionitio-py -h 
usage: bionitio-py [-h] [--minlen N] [--version] [--log LOG_FILE]
                  [FASTA_FILE [FASTA_FILE ...]]

Print fasta stats

positional arguments:
  FASTA_FILE      Input FASTA files

optional arguments:
  -h, --help      show this help message and exit
  --minlen N      Minimum length sequence to include in stats (default 0)
  --version       show program's version number and exit
  --log LOG_FILE  record program progress in LOG_FILE
```

## Reading FASTA files named on the command line

Bionitio accepts zero or more named FASTA files on the command line. These must be specified following all other command line arguments. If zero files are named, bionitio will read a single FASTA file from the standard input device (stdin).

There are no restrictions on the name of the FASTA files. Often FASTA filenames end in `.fa` or `.fasta`, but that is merely a convention, which is not enforced by bionitio. 

The example below illustrates bionitio applied to a single named FASTA file called `file1.fa`:
```
$ bionitio-py file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
```

The example below illustrates bionitio applied to three named FASTA files called `file1.fa`, `file2.fa` and `file3.fa`:
```
$ bionitio-py file1.fa file2.fa file3.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
file2.fa	5264	3801855	31	722	53540
file3.fa	5264	3801855	31	722	53540
```

## Reading a single FASTA file from standard input 

The example below illustrates bionitio reading a FASTA file from standard input. In this example we have redirected the contents of a file called `file1.fa` into the standard input using the shell redirection operator `<`:

```
$ bionitio-py < file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

Equivalently, you could achieve the same result by piping a FASTA file into bionitio:

```
$ cat file1.fa | bionitio-py
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

## Filtering sequences by length 

Bionitio provides an optional command line argument `--minlen` which causes it to ignore (not count) any sequences in the input FASTA files with length strictly less than the supplied value. 

The example below illustrates bionitio applied to a single FASTA file called `file`.fa` with a `--minlen` filter of `1000`.
```
$ bionitio-py --minlen 1000 file.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	4711	2801855	1021	929	53540
```

## Empty files

It is possible that the input FASTA file contains zero sequences, or, when the `--minlen` command line argument is used, it is possible that the file contains no sequences of length greater-than-or-equal-to the supplied value. In both of those cases bionitio will not be able to compute minimum, maximum or average sequence lengths, and instead it shows output in the following way:

The example below illustrates bionitio applied to a single FASTA file called `empty`.fa` which contains zero sequences:
```
$ bionitio-py empty.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
empty.fa	0	0	-	-	-
```

## Logging

If the ``--log FILE`` command line argument is specified, bionitio will output a log file containing information about program progress. The log file includes the command line used to execute the program, and a note indicating which files have been processed so far. Events in the log file are annotated with their date and time of occurrence. 

```
% bionitio-py --log bt.log file1.fasta file2.fasta 
# normal bionitio output appears here
# contents of log file displayed below
% cat bt.log
12/04/2016 19:14:47 INFO - program started
12/04/2016 19:14:47 INFO - command line: bionitio-py --log bt.log file1.fasta file2.fasta 
12/04/2016 19:14:47 INFO - Processing FASTA file from file1.fasta
12/04/2016 19:14:47 INFO - Processing FASTA file from file2.fasta
```

# Exit status values

Bionitio returns the following exit status values:

* 0: The program completed successfully.
* 1: File I/O error. This can occur if at least one of the input FASTA files cannot be opened for reading. This can occur because the file does not exist at the specified path, or bionitio does not have permission to read from the file. 
* 2: A command line error occurred. This can happen if the user specifies an incorrect command line argument. In this circumstance bionitio will also print a usage message to the standard error device (stderr).
* 3: Input FASTA file is invalid. This can occur if bionitio can read an input file but the file format is invalid. 

# Error handling

## Invalid input FASTA files

## Incorrect command line arguments

## Memory limits and other resource restrictions

# Testing

A set of sample test input files is provided in the `test_data` folder. Additionally, each implementation of bionitio comes with its own testing facilities which utilise features and libraries of the particular programming language. Instructions for language-specific testing are provided in the README.md files for each implementation.

# Programming languages used to implement bionitio

* C
* C++
* C#
* Clojure
* Java
* Javascript
* Haskell
* Perl5
* Python
* R
* Ruby
* Rust

# Bugs

File at our [Issue Tracker](https://github.com/bionitio-team/bionitio/issues)

# Authors

Alphabetically:

* Jessica Chung
* Harriet Dashnow
* Peter Georgeson
* Andrew Lonsdale
* Michael Milton
* Bernie Pope
* David R Powell
* Torsten Seemann
