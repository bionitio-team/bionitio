# Ruby biotool

Run without installing: `./bin/biotool`

## How to build & install

1. Ensure you have ruby installed
2. `gem build biotool.gemspec`
3. `gem install --user-install biotool`
4. `export PATH=$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH`

## Running tests

`ruby test/test_biotool.rb`

## Usage

```
biotool --help

biotool FASTA_FILE
```
