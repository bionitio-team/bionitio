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

import org.apache.commons.io.input.CountingInputStream;

import org.biojava.nbio.core.sequence.ProteinSequence;
import org.biojava.nbio.core.sequence.io.FastaReaderHelper;

/**
 * Calculates statistics for a FastaFile, either from an input stream or file.
 */
public class FastaStats {
    /**
     * Construct a FastaStats object from an input stream.
     * @param input the input stream to read fasta sequences from
     * @param logger log progress
     * @param minlength include stats for sequences that are at least this long
     * @throws java.io.IOException if an error occurs while reading the input
     * @throws FastaException if an error occurs while processing the file
     */
    public FastaStats(final InputStream input,
        final Logger logger,
        final int minlength)
        throws java.io.IOException, FastaException {
        log("Reading from stdin...", logger);
        process(input, logger, minlength);
        log("Reading from stdin: done", logger);
    }

    /**
     * Construct a FastaStats object from an input stream.
     * @param filename filename to read fasta sequences from
     * @param logger log progress
     * @param minlength include stats for sequences that are at least this long
     * @throws java.io.IOException if an error occurs while reading the file
     * @throws FastaException if an error occurs while processing the file
     */
    public FastaStats(final String filename,
        final Logger logger,
        final int minlength)
        throws java.io.IOException, FastaException {
        log("Reading " + filename + "...", logger);
        process(new FileInputStream(filename), logger, minlength);
        log("Reading " + filename + ": done", logger);
    }

    /**
     * Calculate stats for a set of fasta sequences read from an input stream.
     * @param input input stream to read fasta sequences from
     * @param logger log progress
     * @param minlength include stats for sequences that are at least this long
     * @throws java.io.IOException if an error occurs while reading the input
     * @throws FastaException if an error occurs while processing the file
     */
    private void process(final InputStream input,
        final Logger logger,
        final int minlength)
        throws java.io.IOException, FastaException {
        LinkedHashMap<String, ProteinSequence> result;
        CountingInputStream countedStream = new CountingInputStream(input);
        try {
            result = FastaReaderHelper.readFastaProteinSequence(countedStream);
        } catch (java.lang.ArrayIndexOutOfBoundsException e) {
            if (countedStream.getCount() == 0) {
                total = 0;
                return;
            }
            throw new FastaException("Failed to process fasta file");
        }
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
     * log a message to stderr if logger is set.
     * @param msg message to log
     * @param logger log progress
     */
    private void log(final String msg, final Logger logger) {
        if (logger != null) {
            logger.log(msg);
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
