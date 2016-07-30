#!/bin/sh

# Install Java dependencies

echo 'Java install'
(
   cd java/biotool
   mvn --quiet compile
   mvn --quiet package

) > /dev/null 2>&1
