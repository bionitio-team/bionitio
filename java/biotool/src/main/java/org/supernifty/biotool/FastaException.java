package org.supernifty.biotool;
/**
 * custom exception when fasta processing fails
 */
public class FastaException extends Exception {
    /**
     * construct an exception with a message.
     * @msg message
     */
    public FastaException(String msg) {
        super(msg);
    }
}
