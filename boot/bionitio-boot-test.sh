#!/bin/bash

# Test the bionitio bootstrap script
# The path to the bootstrap script is passed in as the first argument

BOOTSTRAP_SCRIPT=$1

# Run the script and create a new python repository
${BOOTSTRAP_SCRIPT} -i python -n skynet -c BSD-3-Clause -a 'Miles Bennett Dyson' -e 'miles@cyberdyne.com' -l logfile

# Install the new program into a virtual environment
python3 -m venv skynet_dev
source skynet_dev/bin/activate
pip install -U skynet 

# Run the test suite

./skynet/functional_tests/skynet-test.sh -p skynet -d ./skynet/functional_tests/test_data -v
