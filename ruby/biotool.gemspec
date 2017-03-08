require File.expand_path("../lib/biotool/version", __FILE__)

Gem::Specification.new do |s|
  s.name        = 'biotool'
  s.version     = Biotool::VERSION
  s.summary     = "Summarise FASTA files"
  s.description = "The program reads one or more input FASTA files. For each file it computes a
variety of statistics, and then prints a summary of the statistics as output."
  s.authors     = ["David Powell"]
  s.email       = 'david@drp.id.au'
  s.files       = ["lib/biotool.rb","lib/biotool/version.rb"]
  s.executables = ["biotool"]
  s.license     = 'MIT'
  s.homepage    = 'https://github.com/biotool-paper/biotool'

  s.add_dependency 'bio', '~> 1.5'
end
