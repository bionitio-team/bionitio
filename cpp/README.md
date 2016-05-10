## How to build on OSX

```
cd biotool/cpp
wget http://packages.seqan.de/seqan-library/seqan-library-2.1.1.tar.xz 
tar xvf seqan-library-2.1.1.tar.xz
mkdir biotool-build
cd biotool-build
cmake .. -DCMAKE_MODULE_PATH=../seqan-library-2.1.1/share/cmake/Modules/ -DSEQAN_INCLUDE_PATH=../seqan-library-2.1.1/include/ -DCMAKE_CXX_FLAGS=-std=c++1
make
```

If you want to use a specific compiler, such as clang++ on OSX then add a flag like so to the `cmake` command above:
```
-DCMAKE_CXX_COMPILER=clang++
```
