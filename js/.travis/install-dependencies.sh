#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   sudo apt-get update && sudo apt-get install npm -y
   cd js
   npm install
)
