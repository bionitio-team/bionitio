#!/bin/bash

set -e
errors=0

cd java/biotool

# Run unit tests
mvn --quiet test > /dev/null || {
    echo "'mvn --quiet test' failed"
    let errors+=1
}

# Check program style
mvn --quiet checkstyle:check > /dev/null || {
    echo "'mvn --quiet checkstyle:check' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Java specific tests"
