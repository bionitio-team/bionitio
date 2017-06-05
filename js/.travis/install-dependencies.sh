#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   sudo apt-get install -y build-essential libssl-dev nodejs
   sudo npm cache clean -f
   sudo npm install -g n
   sudo n stable
   npm -v
   node -v
   which node
   which nodejs
   cd js
   npm install
)
