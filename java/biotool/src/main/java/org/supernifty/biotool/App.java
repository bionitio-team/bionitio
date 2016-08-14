/**
* Implementation of biotool functionality
*
* @author Peter Georgeson
* @version 1.0
*/
package org.supernifty.biotool;

import java.io.PrintStream;
import java.io.InputStream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

/**
 * Main entry point for the biotool application.
 * This class handles the command line interface for biotool,
 * by parsing any command line arguments.
 *
 * The class also handles printing help and version information.
 */
public final class App {

    /** success. */
    public static final int EXIT_OK = 0;
    /** io error. */
    public static final int EXIT_IO = 1;
    /** invalid command line. */
    public static final int EXIT_CMD_LINE = 2;
    /** invalid fasta format. */
    public static final int EXIT_FORMAT = 3;

    /**
     * private constructor prevents instantiation.
     */
    private App() {
    }

    /**
     * write help message to stdout.
     * @param target where to write help
     */
    private static void printHelp(final PrintStream target) {
        target.println(
            "Synopsis:\n"
            + "  Print fasta stats\n"
            + "Usage:\n"
            + "  biotool [options] contigs.fasta [another.fa ...]\n"
            + "Options:\n"
            + "  --help       Show this help\n"
            + "  --version    Print version and exit\n"
            + "  --verbose    Print more stuff about what's happening\n"
            + "  --minlen N   Minimum length sequence to include in stats "
            + "(default=0)"
        );
    }

    /**
     * write version information to stdout.
     * @param target where to write version
     */
    private static void printVersion(final PrintStream target) {
        Package pkg = Package.getPackage("org.supernifty.biotool");
        String version = pkg.getImplementationVersion();
        target.println("biotool version " + version);
    }

    /**
     * process request based on incoming arguments.
     * @param args command line arguments
     * @param in where to read input from
     * @param out where to send output to
     * @param err where to send logging and errors
     * @return exit code indicating result of processing
     */
    public static int process(final String[] args,
                              final InputStream in,
                              final PrintStream out,
                              final PrintStream err) {
        // parse args
        Options options = new Options();
        options.addOption("h", "help", false, "Show this help");
        options.addOption(OptionBuilder.withLongOpt("version")
                                .withDescription("Print version and exit")
                                .create());
        options.addOption("v", "verbose", false,
            "Print more stuff about what's happening");
        options.addOption("m", "minlen", true,
            "Minimum length sequence to include in stats (default=0)");

        CommandLineParser parser = new DefaultParser();
        try {
            CommandLine cmd = parser.parse(options, args);
            if (cmd.hasOption("help")) {
                printHelp(err);
                return EXIT_OK; // success
            }
            if (cmd.hasOption("version")) {
                printVersion(err);
                return EXIT_OK; // success
            }
            // minlen param
            int minlength = 0;
            if (cmd.hasOption("minlen")) {
                try {
                    minlength = Integer.parseInt(
                        cmd.getOptionValue("minlen", "0"));
                } catch (NumberFormatException e) {
                    err.println("\n*** Invalid minlen option. "
                        + "Expected number, got '"
                        + cmd.getOptionValue("minlen", "0") + "' ***\n");
                    printHelp(err);
                    return EXIT_CMD_LINE; // cmd line error
                }
            }
            out.println("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX");
            if (cmd.getArgs().length == 0) {
                try {
                    FastaStats stats = new FastaStats(
                        in, cmd.hasOption("verbose"), minlength);
                    out.printf("%s\t%d\t%d\t%d\t%d\t%d\n", "stdin",
                        stats.getTotal(),
                        stats.getNumSeq(),
                        stats.getMin(),
                        stats.getAverage(),
                        stats.getMax());
                } catch (java.io.IOException e) {
                    err.println("Failed to read stdin: "
                        + e.getMessage());
                    return EXIT_IO; // io error
                }
            } else {
                // process files
                for (String filename: cmd.getArgs()) {
                    try {
                        FastaStats stats = new FastaStats(filename,
                            cmd.hasOption("verbose"),
                            minlength);
                        out.printf("%s\t%d\t%d\t%d\t%d\t%d\n",
                            filename,
                            stats.getTotal(),
                            stats.getNumSeq(),
                            stats.getMin(),
                            stats.getAverage(), stats.getMax());
                    } catch (java.io.IOException e) {
                        err.println("Failed to open '"
                            + filename
                            + "': " + e.getMessage());
                        return EXIT_IO; // io error
                    }
                }
            }
        } catch (FastaException e) {
            err.println("\n*** Invalid input file: "
                + e.getMessage()
                + " ***\n");
            return EXIT_FORMAT; // invalid fasta
        } catch (ParseException e) {
            err.println("\n*** Invalid command line arguments: "
                + e.getMessage()
                + " ***\n");
            printHelp(err);
            return EXIT_CMD_LINE; // cmd line error
        }
        return EXIT_OK;
    }

    /**
     * command line entry point.
     * @param args command line arguments
     */
    public static void main(final String[] args) {
        int result = process(args, System.in, System.out, System.err);
        System.exit(result);
    }
}
