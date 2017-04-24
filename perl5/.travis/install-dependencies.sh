#!/bin/sh

# Install Perl5 dependencies

echo 'Perl5 install'
(
   sudo apt-get update
   sudo apt-get install -y bioperl
   sudo apt-get install -y liblog-log4perl-perl 
   sudo apt-get install -y cpanminus 
   sudo cpanm Getopt::ArgParse
)
