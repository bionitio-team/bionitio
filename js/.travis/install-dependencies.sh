#!/bin/sh

# Install Javascript dependencies 
# Install node modules

echo 'JS install'
(
   sudo apt-get update && sudo apt-get install npm -y
   cd js
   npm install
)
