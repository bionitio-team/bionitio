#!/bin/sh

# Install Rust dependencies

echo 'Rust install'
(
   sudo curl -sSf https://static.rust-lang.org/rustup.sh | sh
   cd rust
   cargo install

)
