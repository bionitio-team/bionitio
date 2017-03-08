#!/bin/sh

# Install Ruby depdendencies

echo 'Ruby install'
(
   cd ruby
   gem install bio
)
