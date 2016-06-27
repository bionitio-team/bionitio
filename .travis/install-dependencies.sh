#!/bin/sh

############
# JS
# Install node modules
(cd js ; npm install)

############
# Ruby
(cd ruby; bundle install)

############
# Haskell
# Install stack from fpcomplete repo
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu precise main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y
(cd haskell; stack setup ; stack install)

############
# Python 
(sudo pip install ./python)
