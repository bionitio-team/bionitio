package org.supernifty.bionitio;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit tests for the bionitio application
 */
public class AppTest 
    extends TestCase {
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public AppTest(String testName) {
        super(testName);
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite() {
        return new TestSuite(AppTest.class);
    }

    /**
     * OK
     */
    public void testProcessOK()
        throws java.io.IOException {
        InputStream in = new ByteArrayInputStream(">header\nATGC\nA".getBytes("UTF-8"));
        ByteArrayOutputStream target = new ByteArrayOutputStream();
        PrintStream out = new PrintStream(target);
        ByteArrayOutputStream errorTarget = new ByteArrayOutputStream();
        PrintStream err = new PrintStream(errorTarget);
        int result = App.process(new String[] {}, in, out, err);
        assertTrue("".equals(new String(errorTarget.toByteArray())));
        assertTrue("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX\nstdin\t1\t5\t5\t5\t5\n".equals(new String(target.toByteArray())));
        assertTrue(result == App.EXIT_OK);
    }

    /**
     * IO
     */
    public void testProcessWrongIO()
        throws java.io.IOException {
        InputStream in = new ByteArrayInputStream("".getBytes("UTF-8"));
        PrintStream out = new PrintStream(new ByteArrayOutputStream());
        ByteArrayOutputStream errorTarget = new ByteArrayOutputStream();
        PrintStream err = new PrintStream(errorTarget);
        int result = App.process(new String[] {"nonexistentfile"}, in, out, err);
        assertTrue(result == App.EXIT_IO);
    }

    /**
     * wrong args
     */
    public void testProcessWrongArgs()
        throws java.io.IOException {
        InputStream in = new ByteArrayInputStream("".getBytes("UTF-8"));
        PrintStream out = new PrintStream(new ByteArrayOutputStream());
        PrintStream err = new PrintStream(new ByteArrayOutputStream());
        int result = App.process(new String[] {"--blah"}, in, out, err);
        assertTrue(result == App.EXIT_CMD_LINE);
    }

    /**
     * Format
     */
    public void testProcessWrongFormat()
        throws java.io.IOException {
        InputStream in = new ByteArrayInputStream("invalidfile\nformat\n".getBytes("UTF-8"));
        PrintStream out = new PrintStream(new ByteArrayOutputStream());
        ByteArrayOutputStream errorTarget = new ByteArrayOutputStream();
        PrintStream err = new PrintStream(errorTarget);
        int result = App.process(new String[] {}, in, out, err);
        assertTrue(result == App.EXIT_FORMAT);
    }
}
