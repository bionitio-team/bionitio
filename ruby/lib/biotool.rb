require 'logger'
require 'bio'

require 'biotool/version'

module Biotool
    # Class to parse, filter and format stats on a fasta file
    class FastaSummary
        # Parse and filter the given fasta file.
        # Valid opts are:
        #   :minlen -> sequences strictly less than this are ignored
        def initialize(file, opts)
          @file=file
          @opts = opts
          @opts ||= {}
          @opts[:minlen] ||= 0
    
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
        def pretty(filename)
            if @n>0
                [filename, @n, @bp, @min, @bp/@n, @max].join("\t")
            else
                [filename,0,0,'-','-','-'].join("\t")
            end
        end
    
      private
        # filter each sequence in the fasta file and collect stats
        def process(seq)
            return if seq.length < @opts[:minlen]
            @min = @max = seq.length if @n==0
            @n += 1
            @bp += seq.length
            @min = [@min, seq.length].min
            @max = [@max, seq.length].max
        end
    end
end
