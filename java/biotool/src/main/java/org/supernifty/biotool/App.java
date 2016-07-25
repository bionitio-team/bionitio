/**
* Implementation of biotool functionality
*
* @author Peter Georgeson
* @version 1.0
*/
package org.supernifty.biotool;

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
    /**
     * private constructor prevents instantiation.
     */
    private App() {
    }

    /**
     * write help message to stdout.
     */
    private static void printHelp() {
        System.out.println(
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
     */
    private static void printVersion() {
        Package pkg = Package.getPackage("org.supernifty.biotool");
        String version = pkg.getImplementationVersion();
        System.out.println("biotool version " + version);
    }

    /**
     * command line entry point.
     * @param args command line arguments
     */
    public static void main(final String[] args) {
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
                printHelp();
                System.exit(0); // success
            }
            if (cmd.hasOption("version")) {
                printVersion();
                System.exit(0); // success
            }
            // minlen param
            int minlength = 0;
            if (cmd.hasOption("minlen")) {
                try {
                    minlength = Integer.parseInt(
                        cmd.getOptionValue("minlen", "0"));
                } catch (NumberFormatException e) {
                    System.err.println("\n*** Invalid minlen option. "
                        + "Expected number, got '"
                        + cmd.getOptionValue("minlen", "0") + "' ***\n");
                    printHelp();
                    System.exit(2); // cmd line error
                }
            }
            System.out.println("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX");
            if (cmd.getArgs().length == 0) {
                try {
                    FastaStats stats = new FastaStats(
                        System.in, cmd.hasOption("verbose"), minlength);
                    System.out.printf("%s\t%d\t%d\t%d\t%d\t%d\n", "stdin",
                        stats.getTotal(),
                        stats.getNumSeq(),
                        stats.getMin(),
                        stats.getAverage(),
                        stats.getMax());
                } catch (java.io.IOException e) {
                    System.err.println("Failed to read stdin: "
                        + e.getMessage());
                    System.exit(1); // io error
                }
            } else {
                // process files
                for (String filename: cmd.getArgs()) {
                    try {
                        FastaStats stats = new FastaStats(filename,
                            cmd.hasOption("verbose"),
                            minlength);
                        System.out.printf("%s\t%d\t%d\t%d\t%d\t%d\n",
                            filename,
                            stats.getTotal(),
                            stats.getNumSeq(),
                            stats.getMin(),
                            stats.getAverage(), stats.getMax());
                    } catch (java.io.IOException e) {
                        System.err.println("Failed to open '"
                            + filename
                            + "': " + e.getMessage());
                        System.exit(1); // io error
                    }
                }
            }
        } catch (FastaException e) {
            System.err.println("\n*** Invalid input file: "
                + e.getMessage()
                + " ***\n");
            System.exit(3); // invalid fasta
        } catch (ParseException e) {
            System.err.println("\n*** Invalid command line arguments: "
                + e.getMessage()
                + " ***\n");
            printHelp();
            System.exit(2); // cmd line error
        }
    }
}
