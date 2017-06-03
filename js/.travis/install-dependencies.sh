#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   sudo apt-get update && sudo apt-get install npm -y
   cd js
   curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
   sudo apt-get install -y nodejs
   sudo npm install -g npm
)
