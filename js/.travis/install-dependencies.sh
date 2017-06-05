#!/bin/sh

# Install Javascript dependencies 
# Install node modules

echo 'JS install'
(
  OLD_NODE=$(which node)
  . $HOME/.nvm/nvm.sh
  cd js
  nvm install stable
  nvm use stable
  npm install
  ln -sf $(which node) $OLD_NODE # hack to get past travis
)
