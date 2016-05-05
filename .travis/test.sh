#!/bin/bash

set -e
errors=0

exe="$1"

$exe -h | grep -q 'Usage:'|| {
    echo "Test Failed.  Expected 'Usage:'"
    let errors+=1
}

res1='test_data/one_sequence.fasta	1	237	237	237	237'
$exe test_data/one_sequence.fasta | grep -q "$res1" || {
    echo "Test Failed.  Expected '$res1'"
    let errors+=1
}


[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : $exe"
