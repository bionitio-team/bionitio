#!/bin/bash

set -e
errors=0

TOP_DIR=`pwd`
cd haskell 

# Run unit tests
stack test > /dev/null 2>&1 || {
    echo "'stack test' failed"
    let errors+=1
}

# Run hlint static analysis 
~/.local/bin/hlint src || {
    echo "'~/.local/bin/hlint src' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Haskell specific tests"
