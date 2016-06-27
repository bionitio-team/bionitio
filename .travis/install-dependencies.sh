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

############
# Rust 
sudo curl -sSf https://static.rust-lang.org/rustup.sh | sh
(cd rust; cargo install)

############
# C++
cd cpp
BIOTOOL_CPP_PATH=`pwd`
wget http://packages.seqan.de/seqan-library/seqan-library-2.1.1.tar.xz 
tar xvf seqan-library-2.1.1.tar.xz
mkdir biotool-build
cd biotool-build
cmake $BIOTOOL_CPP_PATH -DCMAKE_MODULE_PATH=$BIOTOOL_CPP_PATH/seqan-library-2.1.1/share/cmake/Modules/ -DSEQAN_INCLUDE_PATH=$BIOTOOL_CPP_PATH/seqan-library-2.1.1/include/ -DCMAKE_CXX_FLAGS=-std=c++11
(make)
