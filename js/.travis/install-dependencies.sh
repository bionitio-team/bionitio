#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
   sudo apt-get install -y nodejs
   npm -v
   node -v
   sudo npm install -g npm
   npm -v
   node -v
   sudo npm install -g npm
   npm -v
   node -v
   which node
   which nodejs
   cd js
   npm install
)
