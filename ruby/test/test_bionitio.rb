require "test/unit"
require 'stringio'

# Useful to add lib dir manually
libdir = File.dirname(__FILE__) + "/../lib"
$LOAD_PATH.unshift(libdir) unless $LOAD_PATH.include?(libdir)

require 'bionitio'

class BionitioTest < Test::Unit::TestCase
  def check(str_in, expect, opts={})
    assert_equal Bionitio::FastaSummary.new(StringIO.new(str_in),opts).pretty(''), expect
  end

  def test_zero_bytes
    check('', "\t0\t0\t-\t-\t-")
  end

  def test_one
    check(">header\nAGTAT", "\t1\t5\t5\t5\t5")
  end

  def test_two
    check(">header\nAGTAT\n>header2\nAGC", "\t2\t8\t3\t4\t5")
  end

  def test_minlen
    check(">header\nAGTAT\n>header2\nAGC", "\t1\t5\t5\t5\t5", {:minlen => 4})
  end
  
  def test_all_filtered
    check(">header\nAGTAT\n>header2\nAGC", "\t0\t0\t-\t-\t-", {:minlen => 8})
  end
end

