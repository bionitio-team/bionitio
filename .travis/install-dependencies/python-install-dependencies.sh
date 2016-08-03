#!/bin/sh

# Install Python dependencies

echo 'Python install'
(
   #virtualenv python/biotool_dev
   #source python/biotool_dev/bin/activate
   pip install --user biopython
   pip install --user pylint
   pip install --user ./python
   
#) > /dev/null 2>&1
) 
