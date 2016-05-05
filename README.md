[![travis](https://travis-ci.org/biotool-paper/biotool.svg?branch=master)](https://travis-ci.org/biotool-paper/biotool)

# biotool

This project will provide a template for implementing command line bioinformatics tools in various languages, 
by demonstrating best practice using a toy example called `biotool`.

## Usage

Help:
```
% biotool -h
Synposis:
  Print fasta stats
Usage:
  biotool [options] contigs.fasta [another.fa ...]
Options:
  --help       Show this help
  --version    Print version and exit
  --verbose    Print more stuff about what's happening
  --minlen N   Minimum length sequence to include in stats (default=0)
```

One file parameter:
```
% biotool file.fa
FILENAME  TOTAL  NUMSEQ   MIN  AVG  MAX
file.fa   5264   3801855  31   722  53540
```

Multiple files:
```
% biotool file1.fa file2.fa file3.fa
FILENAME   TOTAL  NUMSEQ   MIN  AVG  MAX
file1.fa   5264   3801855  31   722  53540
file2.fa   5264   3801855  31   722  53540
file3.fa   5264   3801855  31   722  53540
```

Standard input:
```
% biotool < file.fa
FILENAME  TOTAL  NUMSEQ   MIN  AVG  MAX
stdin     5264   3801855  31   722  53540
```

Restricted length:
```
% biotool --minlen 1000 file.fa
FILENAME  TOTAL  NUMSEQ   MIN    AVG  MAX
file.fa   4711   2801855  1021   929  53540
```

## Languages

* Python
* Perl5
* Ruby
* R
* Javascript
* C

## Licence

[MIT License](https://raw.githubusercontent.com/biotool-paper/biotool/master/LICENSE)

## Bugs

File at our [Issue Tracker](https://github.com/biotool-paper/biotool/issues)

## Authors

Alphabetically:

* Harriet Dashnow
* Andrew Lonsdale
* Bernard J Pope
* David R Powell
* Torsten Seemann
