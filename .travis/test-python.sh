#!/bin/bash

set -e
errors=0

cd python

(
   cd biotool
   # Run unit tests
   python -m unittest -v biotool || {
       echo "'python -m unittest -v biotool' failed"
       let errors+=1
   }
)

# Check program style
pylint -E biotool || {
    echo "'pylint -E biotool' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Python specific tests"
