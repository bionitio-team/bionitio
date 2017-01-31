[![travis](https://travis-ci.org/biotool-paper/biotool.svg?branch=master)](https://travis-ci.org/biotool-paper/biotool)

# Overview 

This project provides a template for implementing command line bioinformatics tools in various programming languages, 
demonstrating best practice using a toy example called `biotool`.

The program reads one or more input FASTA files. For each file it computes a variety of simple statistics, and then prints a summary output.

The goal is to provide a solid foundation for new bioinformatics command line tools, and is an ideal starting place for new projects. An additional advantage of biotool is that it allows us to compare programming styles in different languages.

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

The biotool project is released as open source software under the terms of [MIT License](https://raw.githubusercontent.com/biotool-paper/biotool/master/LICENSE).
However, we grant permission to users who derive their own projects from biotool to apply their own license to their derived works. Licenses applied to projects deriving from biotool do not affect in any way the license of the overall biotool project, or licenses applied to other independent derivations.

# Starting a new project from biotool

One of the main goals of biotool is to provide a good place to start writing bioinformatics command line tools. To make that easy we've provided a shell script to help you start a new project. You must specify the following things: 

Required:

* -l \<language\>: the programming language you want to use (one of: c, clojure, cpp, haskell, java, js, perl5, python, r, ruby, rust)
* -n \<name\>: the name of your new project.

Optional:

* -c \<license\>: the license that you want to assign to your new project (one of: Apache-2.0, BSD-2-Clause, BSD-3-Clause, GPL-2.0, GPL-3.0, MIT). If you do not specify a license then it defaults to the MIT license.

You can run the script like so, using curl:

```
curl -sSf https://raw.githubusercontent.com/biotool-paper/biotool/master/boot/biotool-boot.sh \
 | bash -s -- -l rust -n skynet -c BSD-3-Clause
```

The example above starts a fresh project called `skynet` using Rust as the implementation language. A new git repository will be created in a sub-directory called `skynet`, and the project using the BSD 3 Clause license.

If you prefer not to run a shell script from the web, then you can clone the biotool repository, and run the script locally, as shown below:

```
curl https://raw.githubusercontent.com/biotool-paper/biotool/master/boot/biotool-boot.sh > biotool-boot.sh
bash biotool-boot.sh -l rust -n skynet -c BSD-3-Clause
```

# General behaviour

Biotool accepts zero or more FASTA filenames on the command line. It processes each file in sequence, computing various simple statistics about the reads contained in the file,
and then displays a tab-delimited summary of the statistics as output. If zero filenames are specified it reads a single FASTA file from the standard input device (stdin). 

An optional command line argument `--minlen` can be supplied. Sequences with length strictly less than the given value will be ignored by biotool and do not contribute to the computed statistics. By default `--minlen` is set to zero.

These are the statistics computed by biotool, for all sequences with length greater-than-or-equal-to `--minlen`:

* NUMSEQ: the number of sequences in the file satisfying the minimum length requirement.
* TOTAL: the total length of all the counted sequences.
* MIN: the minimum length of the counted sequences.
* AVERAGE: the average length of the counted sequences rounded down to an integer.
* MAX: the maximum length of the counted sequences.

If there are zero sequences counted in a file, the values of MIN, AVERAGE and MAX cannot be computed. In that situation biotool will print a dash (`-`) in the place of the numerical values. Note that when `--minlen` is set to a value greater than zero it is possible that an input FASTA file does not contain any sequences with length greater-than-or-equal-to the specified value. If this situation arises biotool acts in the same way as if there are no sequences in the file.

Biotool processes each FASTA file one sequence at a time. Therefore the memory usage is proportional to the longest sequence in the file.

# Installation and usage 

Installation and usage instructions are provided separately for each of the implementations of biotool. Please see the README.md files in the corresponding sub-folders for each implementation.

Each implementation of biotool conforms to a standard command line interface, illustrated below. There may be small cosmetic differences in help messages due to programming language idiosyncrasies, but otherwise the behaviour of all implementations should be the same. The name of the executable program for each version is different, for example, the Python version is called `biotool-py`. The examples below show the output from the Python version, however the other implementations will behave similarly.

In the examples below, `$` indicates the command line prompt.

## Help message

Biotool can display usage information on the command line via the `-h` or `--help` argument:
```
$ biotool-py -h 
usage: biotool-py [-h] [--minlen N] [--version] [--log LOG_FILE]
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

Biotool accepts zero or more named FASTA files on the command line. These must be specified following all other command line arguments. If zero files are named, biotool will read a single FASTA file from the standard input device (stdin).

There are no restrictions on the name of the FASTA files. Often FASTA filenames end in `.fa` or `.fasta`, but that is merely a convention, which is not enforced by biotool. 

The example below illustrates biotool applied to a single named FASTA file called `file1.fa`:
```
$ biotool-py file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
```

The example below illustrates biotool applied to three named FASTA files called `file1.fa`, `file2.fa` and `file3.fa`:
```
$ biotool-py file1.fa file2.fa file3.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
file2.fa	5264	3801855	31	722	53540
file3.fa	5264	3801855	31	722	53540
```

## Reading a single FASTA file from standard input 

The example below illustrates biotool reading a FASTA file from standard input. In this example we have redirected the contents of a file called `file1.fa` into the standard input using the shell redirection operator `<`:

```
$ biotool-py < file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

Equivalently, you could achieve the same result by piping a FASTA file into biotool:

```
$ cat file1.fa | biotool-py
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

## Filtering sequences by length 

Biotool provides an optional command line argument `--minlen` which causes it to ignore (not count) any sequences in the input FASTA files with length strictly less than the supplied value. 

The example below illustrates biotool applied to a single FASTA file called `file`.fa` with a `--minlen` filter of `1000`.
```
$ biotool-py --minlen 1000 file.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	4711	2801855	1021	929	53540
```

## Empty files

It is possible that the input FASTA file contains zero sequences, or, when the `--minlen` command line argument is used, it is possible that the file contains no sequences of length greater-than-or-equal-to the supplied value. In both of those cases biotool will not be able to compute minimum, maximum or average sequence lengths, and instead it shows output in the following way:

The example below illustrates biotool applied to a single FASTA file called `empty`.fa` which contains zero sequences:
```
$ biotool-py empty.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
empty.fa	0	0	-	-	-
```

## Logging

If the ``--log FILE`` command line argument is specified, biotool will output a log file containing information about program progress. The log file includes the command line used to execute the program, and a note indicating which files have been processes so far. Events in the log file are annotated with their date and time of occurrence. 

```
% biotool-py --log bt.log file1.fasta file2.fasta 
# normal biotool output appears here
# contents of log file displayed below
% cat bt.log
12/04/2016 19:14:47 INFO - program started
12/04/2016 19:14:47 INFO - command line: biotool-py --log bt.log file1.fasta file2.fasta 
12/04/2016 19:14:47 INFO - Processing FASTA file from file1.fasta
12/04/2016 19:14:47 INFO - Processing FASTA file from file2.fasta
```

# Exit status values

Biotool returns the following exit status values:

* 0: The program completed successfully.
* 1: File I/O error. This can occur if at least one of the input FASTA files cannot be opened for reading. This can occur because the file does not exist at the specified path, or biotool does not have permission to read from the file. 
* 2: A command line error occurred. This can happen if the user specifies an incorrect command line argument. In this circumstance biotool will also print a usage message to the standard error device (stderr).
* 3: Input FASTA file is invalid. This can occur if biotool can read an input file but the file format is invalid. 

# Error handling

## Invalid input FASTA files

## Incorrect command line arguments

## Memory limits and other resource restrictions

# Testing

A set of sample test input files is provided in the `test_data` folder. Additionally, each implementation of biotool comes with its own testing facilities which utilise features and libraries of the particular programming language. Instructions for language-specific testing are provided in the README.md files for each implementation.

# Programming languages used to implement biotool

* C
* C++
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

File at our [Issue Tracker](https://github.com/biotool-paper/biotool/issues)

# Authors

Alphabetically:

* Jessica Chung
* Harriet Dashnow
* Peter Georgeson
* Andrew Lonsdale
* Bernie Pope
* David R Powell
* Torsten Seemann
