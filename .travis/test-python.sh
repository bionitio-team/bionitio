#!/bin/bash

set -e
errors=0

TOP_DIR=`pwd`
cd python

# Run unit tests
cd biotool
python biotool_test -v || {
    echo "'python -m unittest -v biotool' failed"
    let errors+=1
}

cd $TOP_DIR
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
