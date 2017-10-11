# Ruby bionitio

Run without installing: `./bin/bionitio`

## How to build & install

1. Ensure you have ruby installed
2. `gem build bionitio.gemspec`
3. `gem install --user-install bionitio`
4. `export PATH=$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH`

## Running tests

`ruby test/test_bionitio.rb`

## Usage

```
bionitio --help

bionitio FASTA_FILE
```
