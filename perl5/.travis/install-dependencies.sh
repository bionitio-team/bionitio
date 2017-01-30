#!/bin/sh

# Install Perl5 dependencies

echo 'Perl5 install'
(
   sudo apt-get update
   sudo apt-get install -y bioperl
)
