#!/bin/sh

# Install Clojure dependencies

echo 'Clojure install'
(
   sudo apt-get update && sudo apt-get install leiningen -y
   cd clojure 
   lein bin
)
