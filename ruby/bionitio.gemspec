require File.expand_path("../lib/bionitio/version", __FILE__)

Gem::Specification.new do |s|
  s.name        = 'bionitio'
  s.version     = Bionitio::VERSION
  s.summary     = "Summarise FASTA files"
  s.description = "The program reads one or more input FASTA files. For each file it computes a
variety of statistics, and then prints a summary of the statistics as output."
  s.authors     = ["David Powell"]
  s.email       = 'david@drp.id.au'
  s.files       = ["lib/bionitio.rb","lib/bionitio/version.rb"]
  s.executables = ["bionitio"]
  s.license     = 'MIT'
  s.homepage    = 'https://github.com/bionitio-team/bionitio'

  s.add_dependency 'bio', '~> 1.5'
end
