#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
. $HOME/.nvm/nvm.sh
nvm install stable
nvm use stable
npm install
which npm
which node
cd js
npm install
