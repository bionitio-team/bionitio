#!/bin/sh

# Install npm
# Install node 

echo 'JS install'
(
   curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
   sudo apt-get install -y nodejs
   sudo apt-get install build-essential
)
