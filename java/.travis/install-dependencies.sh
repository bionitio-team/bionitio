#!/bin/sh

# Install Java dependencies

echo 'Java install'
(
   cd java/biotool
   mvn --quiet compile
   mvn --quiet package

)
