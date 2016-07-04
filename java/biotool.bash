#!/bin/bash

# Wrapper around jar file
# TODO: Change memory handling

JAR_LOC="$(dirname $0)/biotool/target/biotool-1.0-jar-with-dependencies.jar"

if [ $# -eq 0 ]
 then
java -Xmx512m -jar $JAR_LOC </dev/stdin
 else
java -Xmx512m -jar $JAR_LOC "$@"
fi

