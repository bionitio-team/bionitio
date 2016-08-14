#!/bin/bash

set -e
errors=0

#TOP_DIR=`pwd`
#cd python

# Run unit tests
python python/biotool/biotool_test.py > /dev/null 2>&1 || {
    echo "'python biotool_test.py' failed"
    let errors+=1
}

#cd $TOP_DIR
# Check program style
# pylint -E python/biotool > /dev/null 2>&1 || {
pylint -E python/biotool || {
    echo "'pylint -E biotool' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Python specific tests"
