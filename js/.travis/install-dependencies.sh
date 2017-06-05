#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
   sudo apt-get install -y build-essential libssl-dev nodejs
   npm -v
   node -v
   nvm ls-remote
   nvm install 8.0.0
   nvm use 8.0.0
   nvm alias default 8.0.0
   npm -v
   node -v
   which node
   which nodejs
   cd js
   npm install
)
