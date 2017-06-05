#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   npm -v
   node -v
   cd js
   npm install
)
