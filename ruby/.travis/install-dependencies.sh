#!/bin/sh

# Install Ruby depdendencies

echo 'Ruby install'
(
   cd ruby
   bundle install

) > /dev/null 2>&1
