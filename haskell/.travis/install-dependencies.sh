#!/bin/sh

# Install Haskell dependencies
# Install stack from fpcomplete repo

echo 'Haskell install'
(
   sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
   echo 'deb http://download.fpcomplete.com/ubuntu precise main' | sudo tee /etc/apt/sources.list.d/fpco.list
   sudo apt-get update && sudo apt-get install stack -y
   cd haskell
   stack setup
   stack install
   sudo apt-get install hlint
) > /dev/null 2>&1
