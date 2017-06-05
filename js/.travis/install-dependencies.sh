#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
   sudo apt-get install -y nodejs
   npm -v
   node -v
   npm install -g npm
   npm -v
   node -v
   npm install -g npm
   npm -v
   node -v
   cd js
   npm install
)
