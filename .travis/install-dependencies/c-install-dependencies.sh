#!/bin/sh

# Install C dependencies
echo 'C install'
(
   cd c
   make
) 2>&1
