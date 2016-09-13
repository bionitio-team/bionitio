#!/bin/sh

set -e

./.travis/test.sh c/biotool-c
./.travis/test.sh js/biotool.js
./.travis/test.sh ~/.local/bin/biotool-hs
./.travis/test.sh ruby/biotool.rb
./.travis/test.sh biotool-py 
./.travis/test.sh java/biotool-java
./.travis/test.sh ~/.cargo/bin/biotool-rs
./.travis/test.sh cpp/src/biotool-build/biotool-cpp 
./.travis/test.sh r/biotool.R
