#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   sudo apt-get install -y build-essential libssl-dev nodejs
   curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.2/install.sh | bash
   export NVM_DIR="$HOME/.nvm"
   [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
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
