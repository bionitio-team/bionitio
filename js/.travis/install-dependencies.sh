#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
. $HOME/.nvm/nvm.sh
cd js
nvm install stable
nvm use stable
npm install
which npm
which node
npm install
