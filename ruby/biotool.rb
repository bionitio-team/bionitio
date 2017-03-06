#!/usr/bin/env ruby

require 'rubygems'
require 'bundler/setup'

require 'logger'
require 'optparse'
require 'bio'

Version='0.1.0.0'

opts = { :minlen => 0,
         :log => nil,
       }
exe = File.basename($0)

o = OptionParser.new do |o|
    o.banner = "Usage:\n  #{exe} [options] contigs.fasta [another.fa ...]"
    o.separator ""
    o.separator "Synopsis:"
    o.separator "  Print fasta stats"
    o.separator ""
    o.separator "Options:"

    o.on('--version', "show program's version number and exit") { puts o.ver; exit }
    o.on('--minlen N', 'Minimum length sequence to include in stats') { |m| opts[:minlen] = m.to_i}
    o.on('--log LOG_FILE', "record program progress in LOG_FILE") { |v| opts[:log] = v }
end

# Parse command line, and exit on a bad option
begin
    cmdline = exe + " " + ARGV.join(' ')
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


# Setup logging
logging = Logger.new(opts[:log])
logging.formatter = proc do |severity, datetime, progname, msg|
  "#{datetime} #{severity} : #{msg}\n"
end
logging.info("command line: #{cmdline}")

puts FastaSummary.hdr
if ARGV.length==0
    # Read from STDIN
    logging.info "Processing FASTA file from stdin"
    summary = FastaSummary.new(STDIN, opts).pretty('stdin')
    puts summary
else
    # Process each fasta file
    ARGV.each do |file|
        logging.info "Processing FASTA file from #{file}"
        begin
            summary = FastaSummary.new(file, opts).pretty(file)
            puts summary
        rescue Errno::ENOENT
            msg = "Unable to open file '#{file}'.  Exiting..."
            STDERR.puts msg
            logging.error(msg)
            exit 1            # Exit code 1 on missing file
        end
    end
end
