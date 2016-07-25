package org.supernifty.biotool;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.nio.charset.Charset;

/**
 * Unit test for simple App.
 */
public class FastaStatsTest 
    extends TestCase {
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public FastaStatsTest(String testName) {
        super(testName);
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite() {
        return new TestSuite(FastaStatsTest.class);
    }

    /**
     * single sequence
     */
    public void testOneSequence() 
        throws java.io.IOException, FastaException {
        InputStream stream = new ByteArrayInputStream(">header\nATGC\nA".getBytes("UTF-8"));
        FastaStats stats = new FastaStats(stream, false, 0);
        assertTrue(stats.getTotal() == 1);
        assertTrue(stats.getNumSeq() == 5);
        assertTrue(stats.getMin() == 5);
        assertTrue(stats.getMax() == 5);
    }

    /**
     * two sequences
     */
    public void testTwoSequences() 
        throws java.io.IOException, FastaException {
        InputStream stream = new ByteArrayInputStream(">header1\nATGC\nAGG\n>header2\nTT\n".getBytes("UTF-8"));
        FastaStats stats = new FastaStats(stream, false, 0);
        assertTrue(stats.getTotal() == 2);
        assertTrue(stats.getNumSeq() == 9);
        assertTrue(stats.getMin() == 2);
        assertTrue(stats.getMax() == 7);
    }

    /**
     * two sequences
     */
    public void testMinLen() 
        throws java.io.IOException, FastaException {
        InputStream stream = new ByteArrayInputStream(">header1\nATGC\nAGG\n>header2\nTT\n".getBytes("UTF-8"));
        FastaStats stats = new FastaStats(stream, false, 3);
        assertTrue(stats.getTotal() == 1);
        assertTrue(stats.getNumSeq() == 7);
        assertTrue(stats.getMin() == 7);
        assertTrue(stats.getMax() == 7);
    }

}
