#!/bin/sh

# Install Java dependencies

echo 'Java install'
(
   cd java/bionitio
   mvn --quiet compile
   mvn --quiet package

)
