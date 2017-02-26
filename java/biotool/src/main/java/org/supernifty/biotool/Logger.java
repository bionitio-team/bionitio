/**
* Logger class
*
* @author Peter Georgeson
* @version 1.0
*/
package org.supernifty.biotool;

import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Calendar;

/**
 * Log messages to a provided output stream.
 */
public final class Logger {
    /**
     * construct a logger instance.
     *
     * @param logStream the stream to write to
     */
    public Logger(final PrintStream logStream) {
        this.stream = logStream;
    }

    /**
     * write a formatted log message.
     *
     * @param msg the message to write
     */
    public void log(final String msg) {
        String timeStamp = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
            .format(Calendar.getInstance().getTime());
        stream.println(timeStamp + ": " + msg);
    }

    /** stream to write messages to. */
    private PrintStream stream;
}

