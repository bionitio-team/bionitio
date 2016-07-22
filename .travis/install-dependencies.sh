#!/bin/sh

############
# JS
# Install node modules
echo 'JS install'
(
   cd js
   npm install

) > /dev/null 2>&1

############
# Ruby
echo 'Ruby install'
(
   cd ruby
   bundle install

) > /dev/null 2>&1

############
# Haskell
# Install stack from fpcomplete repo
echo 'Haskell install'
(
   sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
   echo 'deb http://download.fpcomplete.com/ubuntu precise main'|sudo tee /etc/apt/sources.list.d/fpco.list
   sudo apt-get update && sudo apt-get install stack -y
   cd haskell
   stack setup
   stack install

) > /dev/null 2>&1

############
# Python 
echo 'Python install'
(
   sudo pip install ./python
   
) > /dev/null 2>&1

############
# Java
echo 'Java install'
(
   cd java/biotool
   mvn compile
   mvn test
   mvn checkstyle:check
   mvn package

) > /dev/null 2>&1

############
# Rust 
echo 'Rust install'
(
   sudo curl -sSf https://static.rust-lang.org/rustup.sh | sh
   cd rust
   cargo install

) > /dev/null 2>&1

############
# C++
echo 'C++ install'
(
   sudo apt-get update
   sudo apt-get install libboost-program-options-dev -y
   cd cpp
   BIOTOOL_CPP_PATH=`pwd`
   wget --no-check-certificate http://www.cmake.org/files/v3.4/cmake-3.4.1.tar.gz
   tar xzf cmake-3.4.1.tar.gz
   cd cmake-3.4.1/
   ./configure
   sudo make install
   cd $BIOTOOL_CPP_PATH
   wget http://packages.seqan.de/seqan-library/seqan-library-2.1.1.tar.xz 
   tar xvf seqan-library-2.1.1.tar.xz
   mkdir biotool-build
   cd biotool-build
   cmake $BIOTOOL_CPP_PATH -DCMAKE_MODULE_PATH=$BIOTOOL_CPP_PATH/seqan-library-2.1.1/share/cmake/Modules/ -DSEQAN_INCLUDE_PATH=$BIOTOOL_CPP_PATH/seqan-library-2.1.1/include/ -DCMAKE_CXX_FLAGS=-std=c++11
   make

) > /dev/null 2>&1
