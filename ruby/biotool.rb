#!/usr/bin/env ruby

require 'rubygems'
require 'bundler/setup'

require 'bio'

opts = { :minlen => 0,
         :verbose => 0,
       }
OptionParser.new do |o|
    exe = File.basename($0)
    o.banner = "Usage:\n  #{exe} [options] contigs.fasta [another.fa ...]"
    o.separator ""
    o.separator "Synopsis:"
    o.separator "  Print fasta stats"
    o.separator ""
    o.separator "Options:"

    o.on('--minlen N', 'Minimum length sequence to include in stats') { |m| opts[:minlen] = m.to_i}
    o.on('--verbose', "Print more stuff about what's happening") { |v| opts[:verbose] += 1 }
    o.parse!
end

class FastaSummary
    def initialize(file, opts)
      @file=file
      @opts = opts

      @n=0
      @bp=0
      fasta = Bio::FastaFormat.open(@file)
      fasta.each do |seq| 
          process(seq)
      end
    end

    def self.hdr
        %w(FILENAME TOTAL NUMSEQ MIN AVG MAX).join("\t")
    end

    def pretty
        return nil if @n==0
        [@file, @n, @bp, @min, @bp/@n, @max].join("\t")
    end

  private
    def process(seq)
        return if seq.length < @opts[:minlen]
        $stderr.puts [@file, seq.entry_id, seq.length ].join("\t") if @opts[:verbose] >= 2
        @n += 1
        @bp += seq.length
        @min = [@min, seq.length]
        @max = [@max, seq.length]
    end
end


files = ARGV.length==0 ? ["/dev/stdin"] : ARGV

puts FastaSummary.hdr
files.each do |file|
    $stderr.puts "Processing: #{file}" if opts[:verbose]>=1
    summary = FastaSummary.new(file, opts).pretty
    if summary
        puts summary
    else
        $stderr.puts "Skipping #{file} - doesn't seem to be FASTA?";
    end
end

