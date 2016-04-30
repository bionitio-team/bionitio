# biotool

This project will provide a template for implementing command line bioinformatics tools in various languages, 
by demonstrating best practice using a toy example called `biotool`.

## Usage

No parameters:
```
% biotool

Synposis:
  Print fasta stats
Usage:
  biotool [options] contigs.fasta
  biotool [options] < contigs.fasta
Options:
  --help get help
  --version print version and exit
  --verbose print more stuff about what's happening do i don't get anxious about it not doing anytjhing
  --min N  min seq size to include
```

One file parameter:
```
% biotool file1.fa
file1.fa seqs=132 min=13 max=45211 mean=21333 n50=32321 total=234234 
```

Multiple files:
```
% biotool file1.fa
file1.fa seqs=132 ....
file2.fa seqs=11 ....
file3.fa seqs=2342 ...
```


Standard input:
```
% biotool < file1.fa
stdin seqs=132 ....
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
