## How to build on OSX

Assuming the full path to `biotool/cpp` is assigned to the shell variable `BIOTOOL_CPP_PATH`.

For example, you might set it like so:

```
export BIOTOOL_CPP_PATH=$HOME/code/biotool/cpp
```

```
cd $BIOTOOL_CPP_PATH 
wget http://packages.seqan.de/seqan-library/seqan-library-2.1.1.tar.xz 
tar xvf seqan-library-2.1.1.tar.xz
mkdir biotool-build
cd biotool-build
cmake $BIOTOOL_CPP_PATH -DCMAKE_MODULE_PATH=$BIOTOOL_CPP_PATH/seqan-library-2.1.1/share/cmake/Modules/ -DSEQAN_INCLUDE_PATH=$BIOTOOL_CPP_PATH/seqan-library-2.1.1/include/ -DCMAKE_CXX_FLAGS=-std=c++11
make
```

If you want to use a specific compiler, such as clang++ on OSX then add a flag like so to the `cmake` command above:
```
-DCMAKE_CXX_COMPILER=clang++
```
