#!/bin/bash

set -e
errors=0

TOP_DIR=`pwd`
cd haskell 

# Run unit tests
#runhaskell -isrc -itest test/Test.hs > /dev/null 2>&1 || {
stack runghc -isrc -itest test/Test.hs || {
    echo "'runhaskell -isrc -itest test/Test.hs' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Haskell specific tests"
