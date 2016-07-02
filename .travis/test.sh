#!/bin/bash

set -e
errors=0

exe="$1"

# Allow output message to contain "Usage" or "Synopsis" in any case
$exe -h | grep -q -i -e 'Usage' -e 'Synopsis' || {
    echo "Test Failed.  Expected 'Usage' or 'Synopsis'"
    let errors+=1
}


res1='test_data/one_sequence.fasta	1	237	237	237	237'
$exe test_data/one_sequence.fasta | grep -q "$res1" || {
    echo "Test Failed $exe.  Expected '$res1'"
    let errors+=1
}

res2='test_data/two_sequence.fasta	2	357	120	178	237'
$exe test_data/two_sequence.fasta | grep -q "$res2" || {
    echo "Test Failed $exe.  Expected '$res2'"
    let errors+=1
}

res3='test_data/two_sequence.fasta	1	237	237	237	237'
$exe --minlen 200 test_data/two_sequence.fasta | grep -q "$res3" || {
    echo "Test Failed $exe.  Expected '$res3'"
    let errors+=1
}



[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : $exe"
