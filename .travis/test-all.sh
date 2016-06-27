#!/bin/sh

set -e

./.travis/test.sh js/biotool.js
./.travis/test.sh ~/.local/bin/biotool-hs
./.travis/test.sh ruby/biotool.rb
./.travis/test.sh biotool-py 
./.travis/test.sh ~/.cargo/bin/biotool-rs
