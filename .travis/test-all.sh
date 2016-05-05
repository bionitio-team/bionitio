#!/bin/sh

set -e

./.travis/test.sh js/biotool.js
./.travis/test.sh ~/.local/bin/biotool-hs-exe
./.travis/test.sh ruby/biotool.rb
