#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   npm -v
   node -v
   which npm
   which node
   echo $PATH
   cd js
   npm install
)
