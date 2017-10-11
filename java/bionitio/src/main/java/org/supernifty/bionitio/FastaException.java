package org.supernifty.bionitio;

/**
 * custom exception when fasta processing fails.
 */
public class FastaException extends Exception {
    /**
     * construct an exception with a message.
     * @param msg message
     */
    public FastaException(final String msg) {
        super(msg);
    }
}
