#!/bin/bash

# Test the bionitio bootstrap script
# The path to the bootstrap script is passed in as the first argument

#BOOTSTRAP_SCRIPT=$1

# Run the script and create a new python repository
#${BOOTSTRAP_SCRIPT} -i python -n biodemo -c BSD-3-Clause -a 'Example Author Name' -e 'example@email.address.com' -l logfile

# Install the new program into a virtual environment
python3 -m venv biodemo_dev
source biodemo_dev/bin/activate
pip install -U ./biodemo 

# Run the test suite

./biodemo/functional_tests/biodemo-test.sh -p biodemo -d ./biodemo/functional_tests/test_data -v
