#!/bin/bash

# Wrapper around jar file
# TODO: Change memory handling

if [ $# -eq 0 ]
 then
java -Xmx512m -jar $(dirname $0)/biotool/target/biotool-1.0-jar-with-dependencies.jar </dev/stdin
 else
java -Xmx512m -jar $(dirname $0)/biotool/target/biotool-1.0-jar-with-dependencies.jar "$*"
fi

