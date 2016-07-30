#!/bin/sh

# Install Javascript dependencies 
# Install node modules

echo 'JS install'
(
   cd js
   npm install

) > /dev/null 2>&1
