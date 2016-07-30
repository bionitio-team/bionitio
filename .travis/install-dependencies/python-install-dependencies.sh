#!/bin/sh

# Install Python dependencies

echo 'Python install'
(
   sudo pip install biopython
   sudo pip install ./python
   sudo pip install pylint
   
) > /dev/null 2>&1
