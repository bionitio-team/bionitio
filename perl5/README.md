# biotool.pl

## Installation

`biotool.pl` depends on [BioPerl](http://bioperl.org) for parsing FASTA
files, and Log4Perl for logging. Installing these can be done in many ways:

### DEB (Ubuntu/Debian/Mint)
```
sudo apt-get install bioperl
```

### RPM (Centos/RHEL/Fedora)
```
sudo yum install perl-bioperl
```

### CPAN (general Unix)
```
sudo cpan Bio::Perl
sudo cpan Log::Log4perl
```

## Usage

```
./biotool.pl FASTA_FILE
```
