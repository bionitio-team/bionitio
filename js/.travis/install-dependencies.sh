#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   rm -rf ~/.nvm 
   git clone https://github.com/creationix/nvm.git ~/.nvm 
   (cd ~/.nvm && git checkout `git describe --abbrev=0 --tags`) 
   source ~/.nvm/nvm.sh 
   nvm install "8.0.0"
   npm -v
   node -v
   which npm
   which node
   cd js
   npm install
)
