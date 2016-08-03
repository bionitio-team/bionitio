#!/bin/sh

# Install Python dependencies

echo 'Python install'
(
   sudo pip install --upgrade pip
   sudo pip install biopython
   sudo pip install pylint
   sudo pip install ./python
   
) > /dev/null 2>&1
