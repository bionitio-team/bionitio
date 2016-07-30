#!/bin/bash

set -e
errors=0

echo "Java specific tests"
cd java/biotool-java

# Run unit tests
mvn test || {
    echo "mvn test failed"
    let errors+=1
}

# Check program style
mvn checkstyle:check || {
    echo "mvn checkstyle:check failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Java specific tests"
