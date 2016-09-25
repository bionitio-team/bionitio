#!/usr/bin/env ruby

require 'rubygems'
require 'bundler/setup'

require 'optparse'
require 'bio'

opts = { :minlen => 0,
         :verbose => 0,
       }
o = OptionParser.new do |o|
    exe = File.basename($0)
    o.banner = "Usage:\n  #{exe} [options] contigs.fasta [another.fa ...]"
    o.separator ""
    o.separator "Synopsis:"
    o.separator "  Print fasta stats"
    o.separator ""
    o.separator "Options:"

    o.on('--minlen N', 'Minimum length sequence to include in stats') { |m| opts[:minlen] = m.to_i}
    o.on('--verbose', "Print more stuff about what's happening") { |v| opts[:verbose] += 1 }
end

# Parse command line, and exit on a bad option
begin
    o.parse! ARGV
rescue OptionParser::InvalidOption => e
    $stderr.puts e
    $stderr.puts o
    exit 2   # Exit code 2 on invalid option
end

# Class to parse, filter and format stats on a fasta file
class FastaSummary
    # Parse and filter the given fasta file.
    # Valid opts are:
    #   :minlen -> sequences strictly less than this are ignored
    #   :verbose -> enable logging
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

    # String format for stats on this fasta file.  Use "-" for invalid numbers when no sequences
    def pretty
        if @n>0
            [@file, @n, @bp, @min, @bp/@n, @max].join("\t")
        else
            [@file,0,0,'-','-','-'].join("\t")
        end
    end

  private
    # filter each sequence in the fasta file and collect stats
    def process(seq)
        return if seq.length < @opts[:minlen]
        $stderr.puts [@file, seq.entry_id, seq.length ].join("\t") if @opts[:verbose] >= 2
        @min = @max = seq.length if @n==0
        @n += 1
        @bp += seq.length
        @min = [@min, seq.length].min
        @max = [@max, seq.length].max
    end
end


# Default to stdin if not files given
files = ARGV.length==0 ? ["/dev/stdin"] : ARGV

puts FastaSummary.hdr
# Process each fasta file
files.each do |file|
    $stderr.puts "Processing: #{file}" if opts[:verbose]>=1
    begin
        summary = FastaSummary.new(file, opts).pretty
        puts summary
    rescue Errno::ENOENT
        exit 1            # Exit code 1 on missing file
    end
end
