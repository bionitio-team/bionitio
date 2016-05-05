
package org.supernifty.biotool;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.FileInputStream;
import java.util.LinkedHashMap;
import org.biojava.nbio.core.sequence.ProteinSequence;
import org.biojava.nbio.core.sequence.io.FastaReaderHelper;

public class FastaStats {
    public FastaStats(InputStream input, boolean verbose, int minlength) throws java.io.IOException {
        log("Reading from stdin...", verbose);
        process( input, verbose, minlength );
        log("Reading from stdin: done", verbose);
    }

    public FastaStats(String filename, boolean verbose, int minlength) throws java.io.IOException {
        log("Reading " + filename + "...", verbose);
        process( new FileInputStream(filename), verbose, minlength );
        log("Reading " + filename + ": done", verbose);
    }

    private void process(InputStream input, boolean verbose, int minlength) throws java.io.IOException {
        LinkedHashMap<String, ProteinSequence> result = FastaReaderHelper.readFastaProteinSequence(input);
        total = 0;
        min = Integer.MAX_VALUE;
        max = 0;
        numseq = 0;
        for (ProteinSequence sequence: result.values()) {
            int sequence_size = sequence.getLength();
            if (sequence_size >= minlength) {
                total += 1;
                numseq += sequence_size;
                min = Math.min(min, sequence_size);
                max = Math.max(max, sequence_size);
            }
        }
    }

    public int getTotal() {
        return total;
    }

    public int getNumSeq() {
        return numseq;
    }

    public int getMin() {
        return min;
    }

    public int getAverage() {
        if (total > 0) {
            return numseq / total;
        }
        else {
            return 0;
        }
    }

    public int getMax() {
        return max;
    }

    private void log(String msg, boolean verbose) {
        if (verbose) {
            System.err.println(msg);
        }
    }

    private int total;
    private int numseq;
    private int min;
    private int max;
}
