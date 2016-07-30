#!/bin/sh

# Install Java dependencies

echo 'Java install'
(
   cd java/biotool
   mvn compile
   mvn test || exit 1
   mvn checkstyle:check || exit 1
   mvn package

) > /dev/null 2>&1
