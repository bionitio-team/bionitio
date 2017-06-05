#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
OLD_NODE=$(which node) # hack to get past travis
. $HOME/.nvm/nvm.sh
cd js
nvm install stable
nvm use stable
npm install
which npm
which node
ln -sf $(which node) $OLD_NODE
npm install
