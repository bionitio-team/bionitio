#!/bin/sh

# Install Python dependencies

echo 'Python install'
(
   sudo pip install ./python
   
) > /dev/null 2>&1
