#!/bin/bash

set -e
errors=0

TOP_DIR=`pwd`
cd python

echo `python --version`

# Run unit tests
cd biotool
python biotool_test.py -v || {
    echo "'python biotool_test.py -v' failed"
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
