#!/bin/sh

# Install Perl5 dependencies

echo 'Perl5 install'
(
   sudo apt-get update
   sudo apt-get install -y bioperl liblog-log4perl-perl cpanminus libreadonly-perl
   sudo cpanm Getopt::ArgParse
)
