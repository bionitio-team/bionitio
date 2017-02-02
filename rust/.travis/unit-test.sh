#!/bin/bash

set -e
errors=0

cd rust 

# Run unit tests
cargo test > /dev/null 2>&1 || {
    echo "'cargo test' failed"
    let errors+=1
}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Rust specific tests"
