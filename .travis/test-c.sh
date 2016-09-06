#!/bin/bash

set -e
errors=0

cd c

# Run unit tests
make test || {
    echo "'make test' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : C specific tests"
