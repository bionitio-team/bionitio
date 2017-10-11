#!/bin/bash

set -e
errors=0

# Run unit tests
(
  cd perl5
  ./bionitio_test.pl || {
    echo "'perl perl5/bionitio/bionitio_test.pl' failed"
    let errors+=1
  }
)

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : Perl specific tests"
