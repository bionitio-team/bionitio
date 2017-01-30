#!/bin/sh

# Install C++ dependencies

echo 'C++ install'
(
   sudo apt-get update
   sudo apt-get install libboost-program-options-dev -y
   sudo apt-get install libboost-log-dev -y
   cd cpp/src
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
)
