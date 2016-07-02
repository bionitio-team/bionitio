package org.supernifty.biotool;

import org.apache.commons.cli.CommandLine; 
import org.apache.commons.cli.CommandLineParser; 
import org.apache.commons.cli.DefaultParser; 
import org.apache.commons.cli.OptionBuilder; 
import org.apache.commons.cli.Options; 
import org.apache.commons.cli.ParseException; 

/**
 * Main entry point for application
 */
public class App 
{

    private static void printHelp() {
        System.err.println(
            "Synopsis:\n" +
            "  Print fasta stats\n" +
            "Usage:\n" +
            "  biotool [options] contigs.fasta [another.fa ...]\n" +
            "Options:\n" +
            "  --help       Show this help\n" +
            "  --version    Print version and exit\n" +
            "  --verbose    Print more stuff about what's happening\n" +
            "  --minlen N   Minimum length sequence to include in stats (default=0)"
        );
    }

    private static void printVersion() {
        System.err.println( "biotool version " + Package.getPackage("org.supernifty.biotool").getImplementationVersion() );
    }

    public static void main( String[] args )
    {
        // parse args
        Options options = new Options();
        options.addOption( "h", "help", false, "Show this help" );
        options.addOption( OptionBuilder.withLongOpt( "version" )
                                .withDescription( "Print version and exit" )
                                .create() );
        options.addOption( "v", "verbose", false, "Print more stuff about what's happening" );
        options.addOption( "m", "minlength", true, "Minimum length sequence to include in stats (default=0)" );

        CommandLineParser parser = new DefaultParser();
        try {
            CommandLine cmd = parser.parse( options, args );
            if ( cmd.hasOption( "help" ) ) {
                printHelp();
                System.exit(0);
            }
            if ( cmd.hasOption( "version" ) ) {
                printVersion();
                System.exit(0);
            }
            // minlength param
            int minlength = 0;
            if ( cmd.hasOption( "minlength" ) ) {
                try {
                    minlength = Integer.parseInt(cmd.getOptionValue("minlength", "0"));
                }
                catch (NumberFormatException e) {
                    System.err.println( "\n*** Invalid minlength option. Expected number, got '" + cmd.getOptionValue("minlength", "0") + "' ***\n" );
                    printHelp();
                    System.exit(0);
                }
            }
            System.out.println("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX");
            if (cmd.getArgs().length == 0) {
                try {
                    FastaStats stats = new FastaStats(System.in, cmd.hasOption("verbose"), minlength);
                    System.out.printf("%s\t%d\t%d\t%d\t%d\t%d\n", "stdin", stats.getTotal(), stats.getNumSeq(), stats.getMin(), stats.getAverage(), stats.getMax());
                }
                catch (java.io.IOException e) {
                    System.err.println("Failed to read stdin: " + e.getMessage());
                }
            }
            else {
                // process files
                for (String filename: cmd.getArgs()) {
                    try {
                        FastaStats stats = new FastaStats(filename, cmd.hasOption("verbose"), minlength);
                        System.out.printf("%s\t%d\t%d\t%d\t%d\t%d\n", filename, stats.getTotal(), stats.getNumSeq(), stats.getMin(), stats.getAverage(), stats.getMax());
                    }
                    catch (java.io.IOException e) {
                        System.err.println("Failed to open '" + filename + "': " + e.getMessage());
                    }
                }
            }
        }
        catch (ParseException e) {
            System.err.println( "\n*** Invalid command line arguments: " + e.getMessage() + " ***\n" );
            printHelp();
        }
    }
}
