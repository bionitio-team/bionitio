/**
* Implementation of biotool functionality
*
* @author Peter Georgeson
* @version 1.0
*/
package org.supernifty.biotool;

import java.io.InputStream;
import java.io.FileInputStream;
import java.util.LinkedHashMap;
import org.biojava.nbio.core.sequence.ProteinSequence;
import org.biojava.nbio.core.sequence.io.FastaReaderHelper;

/**
 * Calculates statistics for a FastaFile, either from an input stream or file.
 */
public class FastaStats {
    /**
     * Construct a FastaStats object from an input stream.
     * @param input the input stream to read fasta sequences from
     * @param verbose write additional details of progress to stderr
     * @param minlength include stats for sequences that are at least this long
     * @throws java.io.IOException if an error occurs while reading the input
     */
    public FastaStats(final InputStream input,
        final boolean verbose,
        final int minlength)
        throws java.io.IOException {
        log("Reading from stdin...", verbose);
        process(input, verbose, minlength);
        log("Reading from stdin: done", verbose);
    }

    /**
     * Construct a FastaStats object from an input stream.
     * @param filename filename to read fasta sequences from
     * @param verbose write additional details of progress to stderr
     * @param minlength include stats for sequences that are at least this long
     * @throws java.io.IOException if an error occurs while reading the file
     */
    public FastaStats(final String filename,
        final boolean verbose,
        final int minlength)
        throws java.io.IOException {
        log("Reading " + filename + "...", verbose);
        process(new FileInputStream(filename), verbose, minlength);
        log("Reading " + filename + ": done", verbose);
    }

    /**
     * Calculate stats for a set of fasta sequences read from an input stream.
     * @param input input stream to read fasta sequences from
     * @param verbose write additional details of progress to stderr
     * @param minlength include stats for sequences that are at least this long
     * @throws java.io.IOException if an error occurs while reading the input
     */
    private void process(final InputStream input,
        final boolean verbose,
        final int minlength)
        throws java.io.IOException {
        LinkedHashMap<String, ProteinSequence> result =
            FastaReaderHelper.readFastaProteinSequence(input);
        total = 0;
        min = Integer.MAX_VALUE;
        max = 0;
        numseq = 0;
        for (ProteinSequence sequence: result.values()) {
            int sequenceSize = sequence.getLength();
            if (sequenceSize >= minlength) {
                total += 1;
                numseq += sequenceSize;
                min = Math.min(min, sequenceSize);
                max = Math.max(max, sequenceSize);
            }
        }
    }

    /**
     * @return total number of sequences found
     */
    public final int getTotal() {
        return total;
    }

    /**
     * @return number of bases found
     */
    public final int getNumSeq() {
        return numseq;
    }

    /**
     * @return length of shortest sequence
     */
    public final int getMin() {
        return min;
    }

    /**
     * @return average length of sequences (or 0 if no sequences found)
     */
    public final int getAverage() {
        if (total > 0) {
            return numseq / total;
        } else {
            return 0;
        }
    }

    /**
     * @return length of longest sequence
     */
    public final int getMax() {
        return max;
    }

    /**
     * log a message to stderr if verbose is set.
     * @param msg message to log
     * @param verbose whether to actually log the message
     */
    private void log(final String msg, final boolean verbose) {
        if (verbose) {
            System.err.println(msg);
        }
    }

    /** total number of bases observed. */
    private int total;
    /** number of sequences seen. */
    private int numseq;
    /** length of shortest sequence. */
    private int min;
    /** length of longest sequence. */
    private int max;
}
