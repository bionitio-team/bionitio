[![travis](https://travis-ci.org/bionitio-team/bionitio-LANGUAGE.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-LANGUAGE)

# Overview 

This program reads one or more input FASTA files. For each file it computes a variety of statistics, and then prints a summary of the statistics as output.

In the examples below, `$` indicates the command line prompt.

# Licence

This program is released as open source software under the terms of [MIT License](https://raw.githubusercontent.com/bionitio-team/bionitio/master/LICENSE).

# Installing

You can install bionitio directly from the source code or build and run it from within Docker container.

## Installing directly from source code

Clone this repository: 
```
$ git clone https://github.com/bionitio-team/bionitio-LANGUAGE
```

Move into the repository directory:
```
$ cd bionitio-LANGUAGE
```

#include "install.md"

## Building the Docker container 

The file `Dockerfile` contains instructions for building a Docker container for bionitio.

If you have Docker installed on your computer you can build the container like so:
```
$ docker build -t bionitio .
```
See below for information about running bionitio within the Docker container.

# General behaviour

Bionitio accepts zero or more FASTA filenames on the command line. If zero filenames are specified it reads a single FASTA file from the standard input device (stdin). Otherwise it reads each named FASTA file in the order specified on the command line. Bionitio reads each input FASTA file, computes various statistics about the contents of the file, and then displays a tab-delimited summary of the statistics as output. Each input file produces at most one output line of statistics. Each line of output is prefixed by the input filename or by the text "`stdin`" if the standard input device was used.

Bionitio processes each FASTA file one sequence at a time. Therefore the memory usage is proportional to the longest sequence in the file.

An optional command line argument `--minlen` can be supplied. Sequences with length strictly less than the given value will be ignored by bionitio and do not contribute to the computed statistics. By default `--minlen` is set to zero.

These are the statistics computed by bionitio, for all sequences with length greater-than-or-equal-to `--minlen`:

* *NUMSEQ*: the number of sequences in the file satisfying the minimum length requirement.
* *TOTAL*: the total length of all the counted sequences.
* *MIN*: the minimum length of the counted sequences.
* *AVERAGE*: the average length of the counted sequences rounded down to an integer.
* *MAX*: the maximum length of the counted sequences.

If there are zero sequences counted in a file, the values of MIN, AVERAGE and MAX cannot be computed. In that case bionitio will print a dash (`-`) in the place of the numerical value. Note that when `--minlen` is set to a value greater than zero it is possible that an input FASTA file does not contain any sequences with length greater-than-or-equal-to the specified value. If this situation arises bionitio acts in the same way as if there are no sequences in the file.

## Help message

Bionitio can display usage information on the command line via the `-h` or `--help` argument:

#include "help.md"

## Reading FASTA files named on the command line

Bionitio accepts zero or more named FASTA files on the command line. These must be specified following all other command line arguments. If zero files are named, bionitio will read a single FASTA file from the standard input device (stdin).

There are no restrictions on the name of the FASTA files. Often FASTA filenames end in `.fa` or `.fasta`, but that is merely a convention, which is not enforced by bionitio. 

The example below illustrates bionitio applied to a single named FASTA file called `file1.fa`:
```
$ bionitio file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
```

The example below illustrates bionitio applied to three FASTA files called `file1.fa`, `file2.fa` and `file3.fa`:
```
$ bionitio file1.fa file2.fa file3.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	5264	3801855	31	722	53540
file2.fa	1245	982374	8	393	928402
file3.fa	64	8376	102	123	212	
```

## Reading a single FASTA file from standard input 

The example below illustrates bionitio reading a FASTA file from standard input. In this example we have redirected the contents of a file called `file1.fa` into the standard input using the shell redirection operator `<`:

```
$ bionitio < file1.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

Equivalently, you could achieve the same result by piping a FASTA file into bionitio:

```
$ cat file1.fa | bionitio
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
stdin	5264	3801855	31	722	53540
```

## Filtering sequences by length 

Bionitio provides an optional command line argument `--minlen` which causes it to ignore (not count) any sequences in the input FASTA files with length strictly less than the supplied value. 

The example below illustrates bionitio applied to a single FASTA file called `file`.fa` with a `--minlen` filter of 1000.
```
$ bionitio --minlen 1000 file.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
file1.fa	4711	2801855	1021	929	53540
```

## Logging

If the ``--log FILE`` command line argument is specified, bionitio will output a log file containing information about program progress. The log file includes the command line used to execute the program, and a note indicating which files have been processes so far. Events in the log file are annotated with their date and time of occurrence. 

```
$ bionitio --log bt.log file1.fasta file2.fasta 
```
#include "log.md"

## Empty files

It is possible that the input FASTA file contains zero sequences, or, when the `--minlen` command line argument is used, it is possible that the file contains no sequences of length greater-than-or-equal-to the supplied value. In both of those cases bionitio will not be able to compute minimum, maximum or average sequence lengths, and instead it shows output in the following way:

The example below illustrates bionitio applied to a single FASTA file called `empty.fa` which contains zero sequences:
```
$ bionitio empty.fa
FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX
empty.fa	0	0	-	-	-
```

## Exit status values

Bionitio returns the following exit status values:

* 0: The program completed successfully.
* 1: File I/O error. This can occur if at least one of the input FASTA files cannot be opened for reading. This can occur because the file does not exist at the specified path, or bionitio does not have permission to read from the file. 
* 2: A command line error occurred. This can happen if the user specifies an incorrect command line argument. In this circumstance bionitio will also print a usage message to the standard error device (stderr).

# Running within the Docker container

The following section describes how to run bionitio within the Docker container. It assumes you have Docker installed on your computer and have built the container as described above. 
The container behaves in the same way as the normal version of bionitio, however there are some Docker-specific details that you must be aware of.

The general syntax for running bionitio within Docker is as follows:
```
$ docker run -i bionitio CMD
```
where CMD should be replaced by the specific command line invocation of bionitio. Specific examples are below.

Display the help message:
```
$ docker run -i bionitio bionitio -h
```
Note: it may seem strange that `bionitio` is mentioned twice in the command. The first instance is the name of the Docker container and the second instance is the name of the bionitio executable that you want to run inside the container.

Display the version number:
```
$ docker run -i bionitio bionitio --version
```

Read from a single input FASTA file redirected from standard input:
```
$ docker run -i bionitio bionitio < file.FASTA 
```

Read from multuple input FASTA files named on the command line, where all the files are in the same directory. You must replace `DATA` with the absolute file path of the directory containing the FASTA files:  
```
$ docker run -i -v DATA:/in bionitio bionitio /in/file1.fasta /in/file2.fasta /in/file3.fasta
```
The argument `DATA:/in` maps the directory called DATA on your local machine into the `/in` directory within the Docker container.

Logging progress to a file in the directory OUT: 
```
$ docker run -i -v DATA:/in -v OUT:/out bionitio-c bionitio --log /out/logfile.txt /in/file1.fasta /in/file2.fasta /in/file3.fasta
```
Replace `OUT` with the absolute path of the directory to write the log file. For example, if you want the log file written to the current working directory, replace `OUT` with `$PWD`.
As above, you will also need to replace `DATA` with the absolite path to the directory containing your input FASTA files.

# Testing

## Unit tests

#include "unit-tests.md"

## Test suite

Sample test input files are provided in the `functional_tests/test_data` folder.
```
$ cd functional_tests/test_data
$ bionitio two_sequence.fasta
FILENAME        TOTAL   NUMSEQ  MIN     AVG     MAX
two_sequence.fasta      2       357     120     178     237
```

Automated tests can be run using the `functional_tests/bionitio-test.sh` script like so:

```
$ cd functional_tests
$ ./bionitio-test.sh -p bionitio -d test_data
```

The `-p` argument specifies the name of the program to test, the `-d` argument specifies the path of the directory containing test data.
The script will print the number of passed and failed test cases. More detailed information about each test case can be obtained
by requesting "verbose" output with the `-d` flag:

```
$ ./bionitio-test.sh -p bionitio -d test_data -v
```

The test script can also be run inside the Docker container:
```
$ docker run bionitio /bionitio/functional_tests/bionitio-test.sh -p bionitio -d /bionitio/functional_tests/test_data -v
```

# Common Workflow Language (CWL) wrapper

The [Common Workflow Language (CWL)](https://www.commonwl.org/) specifies a portable mechanism for running software tools and workflows across many different platforms.
We provide an example CWL wrapper for bionitio in the file `bionitio.cwl`. It invokes bionitio using the Docker container (described above). This wrapper allows you
to easily incorporate bionitio into CWL workflows, and can be executed by any CWL-supporting workflow engine.

You can test the wrapper using the `cwltool` workflow runner, which is provided by the CWL project (see the CWL documentation for how to install this on your computer).

```
$ cwltool bionitio.cwl --fasta_file file.fasta 
```

# Bug reporting and feature requests

Please submit bug reports and feature requests to the issue tracker on GitHub:

[bionitio issue tracker](https://github.com/bionitio-team/bionitio/issues)
