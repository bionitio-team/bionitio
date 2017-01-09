/// Module      : Main
/// Description : The main entry point for the program.
/// Copyright   : (c) Bernie Pope, 2016
/// License     : MIT
/// Maintainer  : bjpope@unimelb.edu.au
/// Stability   : experimental
/// Portability : POSIX
///
/// Read a single FASTA file as input and compute:
///  * Num sequences.
///  * Total number of bases in all sequences.
///  * Minimum sequence length.
///  * Maximum sequence length.
///  * Average sequence length, as an integer, rounded towards zero.
///
/// Sequences whose length is less than a specified minimum length are
/// ignored (skipped) and not included in the calculation of the statistics.
///
/// The basic statistics cannot be computed for empty files, and those
/// with no reads that meet the mimimum length requirement. In such cases
/// the result is Nothing.
///
/// We do not make any assumptions about the type of data stored in the
/// FASTA file, it could be DNA, proteins or anything else. No checks are
/// made to ensure that the FASTA file makes sense biologically.
///
/// Whitespace within the sequences will be ignored.

extern crate bio;
extern crate argparse;
#[macro_use]
extern crate log;
extern crate log4rs;
use std::env;
use std::io;
use std::io::Write;
use std::cmp;
use bio::io::fasta;
use std::fmt;
use std::fs::File;
use argparse::{ArgumentParser, StoreTrue, Store, Print, Collect, StoreOption};
use log4rs::append::file::FileAppender;
use log4rs::encode::pattern::PatternEncoder;
use log4rs::config::{Appender, Config, Root, Logger};
use log::LogLevelFilter;

// File I/O error. This can occur if at least one of the input FASTA
// files cannot be opened for reading. This can occur because the file
// does not exist at the specified path, or biotool does not have
// permission to read from the file.
const EXIT_FILE_IO_ERROR: i32 = 1;

// A command line error occurred. This can happen if the user specifies
// an incorrect command line argument. In this circumstance biotool will
// also print a usage message to the standard error device (stderr).
const EXIT_COMMAND_LINE_ERROR: i32 = 2;

// Input FASTA file is invalid. This can occur if biotool can read an
// input file but the file format is invalid.
const EXIT_FASTA_PARSE_ERROR: i32 = 3;

// Name of the program, to be used in diagnostic messages.
static PROGRAM_NAME: &'static str = "biotool";

/// Exit the program, printing an error message on stderr, and returning
/// a specific error code. The program name is prefixed onto the front of
/// the error message.
fn exit_with_error(status: i32, message: &String) -> () {
    writeln!(&mut std::io::stderr(),
             "{} ERROR: {}!",
             PROGRAM_NAME,
             message)
        .unwrap();
    std::process::exit(status);
}

/// Basic statistics computed for a FASTA file.
/// Note that all values are with respect to only those reads whose
/// length is at least as long as the minimum.
#[derive(Debug, PartialEq)]
pub struct FastaStats {
    /// Minimum length of all sequences in the file.
    min_len: u64,
    /// Average length of all sequences in the file rounded towards zero.
    average_len: u64,
    /// Maximum length of all sequences in the file.
    max_len: u64,
    /// Total number of bases from all the sequences in the file.
    total: u64,
    /// Total number of sequences in the file.
    num_seqs: u64,
}

/// Construct a FastaStats value from an input FASTA file. Sequences
/// of length < `minlen` are ignored.
/// The input FASTA file is represented as an `io::Read` reader.
/// The result is of type `Result<Option<FastaStats>, io::Error>`
///  * `Err(error)` if an error occurred while reading the FASTA file.
///  * `Ok(Nothing)` if the FASTA file had no sequences of length >= `minlen`.
///  * `Ok(Some(stats))` if there were no errors reading the file and there was
///     at least one sequence in the file whose length was >= `minlen`.
impl FastaStats {
    pub fn new<R: io::Read>(minlen: u64, reader: R) -> Result<Option<FastaStats>, io::Error> {
        let fasta_reader = fasta::Reader::new(reader);
        let mut num_seqs: u64 = 0;
        let mut total: u64 = 0;
        let mut max_len: u64 = 0;
        let mut min_len: u64 = 0;
        let mut this_len: u64;

        // Read each sequence in the input FASTA file.
        for next in fasta_reader.records() {
            match next {
                // The sequence was parsed correctly.
                Ok(record) => {
                    // Filter out sequences whose length is too short.
                    this_len = record.seq().len() as u64;
                    if this_len >= minlen {
                        num_seqs += 1;
                        total += this_len;
                        if num_seqs == 1 {
                            // This is the first sequence we have
                            // encountered, set max and min to the
                            // length of this sequence.
                            max_len = this_len;
                            min_len = this_len;
                        } else {
                            // Update max and min.
                            max_len = cmp::max(max_len, this_len);
                            min_len = cmp::min(min_len, this_len);
                        }
                    }
                }
                // There was an error parsing the sequence.
                Err(error) => return Err(error),
            }
        }
        if num_seqs > 0 {
            // We encountered at least one sequence, so we can
            // compute statistics for this file.
            let average_len = ((total as f64) / (num_seqs as f64)).floor() as u64;
            Ok(Some(FastaStats {
                min_len: min_len,
                average_len: average_len,
                max_len: max_len,
                total: total,
                num_seqs: num_seqs,
            }))
        } else {
            // We did not encounter any sequences (satisfying the length
            // requirement). So we cannot compute statistics for this file.
            Ok(None)
        }
    }
}

/// Format the FastaStats for output.
/// Use a tab as a delimiter.
impl fmt::Display for FastaStats {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{}\t{}\t{}\t{}\t{}",
               self.num_seqs,
               self.total,
               self.min_len,
               self.average_len,
               self.max_len)
    }
}

/// Command line arguments supplied to the program.
struct CmdOptions {
    /// If True, make the program produce more detailed output
    /// about its progress.
    verbose: bool,
    /// Minimum length sequence considered by the program. Sequences
    /// shorter than this length are ignored.
    minlen: u64,
    /// Possibly empty vector of input FASTA file paths.
    fasta_files: Vec<String>,
    /// Name of log file if requested.
    log_file: Option<String>
}

/// Parse the command line options.
/// Program exits with status 2 on parsing failure.
fn parse_options() -> CmdOptions {
    let mut options = CmdOptions {
        verbose: false,
        minlen: 0,
        fasta_files: Vec::new(),
        log_file: None
    };
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Print fasta stats");
        ap.refer(&mut options.verbose)
            .add_option(&["-v", "--verbose"],
                        StoreTrue,
                        "Print more stuff about what's happening");
        ap.refer(&mut options.minlen)
            .add_option(&["--minlen"],
                        Store,
                        "Minimum length sequence to include in stats");
        ap.refer(&mut options.log_file)
            .add_option(&["--log"], StoreOption, "Log file name");
        ap.refer(&mut options.fasta_files)
            .add_argument(&"fasta_files", Collect, "Input FASTA files");
        ap.add_option(&["--version"],
                      Print(env!("CARGO_PKG_VERSION").to_string()),
                      "Show version");
        match ap.parse_args() {
            Ok(()) => (),
            Err(0) => {
                // Help and version commands
                std::process::exit(0);
            }
            Err(_error_code) => {
                // Parse error
                std::process::exit(EXIT_COMMAND_LINE_ERROR);
            }
        }
    }
    return options;
}

/// Compute basic statistics for the input FASTA files,
/// and pretty print the results to standard output.
/// Program exits if a parse error occurs when reading an
/// input FASTA file.
fn compute_print_stats<R: io::Read>(options: &CmdOptions, filename: &String, reader: R) -> () {
    match FastaStats::new(options.minlen, reader) {
        Ok(Some(stats)) => {
            // Prefix the FASTA filename onto the front of the statistics
            println!("{}\t{}", filename, stats);
        }
        Ok(None) => {
            // We could not compute any statistics for the file because
            // it contained no sequences at least as long as the minimum
            // length. In this case we just print dashes.
            println!("{}\t0\t0\t-\t-\t-", filename);
        }
        // There was a parse error when reading a FASTA file.
        // Exit the program.
        Err(error) => exit_with_error(EXIT_FASTA_PARSE_ERROR, &format!("{}", error)),
    }
}

/// Initialise the logging infrastructure. If the command line option --log
/// is specified, we set up logging to write to the filename supplied as
/// an argument. If it is not supplied then logging will not occur. Log
/// messages are tagged with their date and time of occurrence.
/// If logging is initialised, then we write a message to indicate that
/// the program started, and then we log the command line arguments.
fn init_logging(options: &CmdOptions) -> () {
    // Check if --log was specified as a command line argument
    match options.log_file {
        // --log was specified, set logging output to the supplied filename
        Some(ref filename) => {
            let log_messages = FileAppender::builder()
                .encoder(Box::new(PatternEncoder::new("{d} - {m}{n}")))
                .build(filename)
                .unwrap();

            let config = Config::builder()
                .appender(Appender::builder().build("log_messages", Box::new(log_messages)))
                .logger(Logger::builder()
                    .appender("log_messages")
                    .additive(false)
                    .build("app::log_messages", LogLevelFilter::Info))
                .build(Root::builder().appender("log_messages").build(LogLevelFilter::Info))
                .unwrap();

            log4rs::init_config(config).unwrap(); 
            info!(target: "log_messages", "program started");
            // collect command line arguments of the program
            let argv: Vec <String> = env::args().collect();
            // log the command line arguments
            info!(target: "log_messages", "command line: {}", argv.join(" "));
        },
        // --log was not specified, do nothing. Log messages will not be written.
        None => ()
    }
}

/// The entry point for the program.
///  * Parse the command line arguments.
///  * Print the output header.
///  * Process each input FASTA file, compute stats and display
///    results.
fn main() {
    let options = parse_options();

    // Optionally initialise the logging system.
    init_logging(&options);

    // Display the output header.
    println!("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX");
    if options.fasta_files.len() == 0 {
        // No FASTA files were specified on the command line, so
        // read from stdin instead.
        info!(target: "log_messages", "Processing FASTA file from stdin");
        compute_print_stats(&options, &String::from("stdin"), io::stdin());
    } else {
        // Process each FASTA file specified on the command line.
        // Exit the program if a file I/O error occurs.
        for filename in &options.fasta_files {
            match File::open(filename) {
                Ok(file) => {
                    info!(target: "log_messages", "Processing FASTA file from {}", filename);
                    compute_print_stats(&options, filename, file);
                }
                Err(error) => exit_with_error(EXIT_FILE_IO_ERROR, &format!("{}", error)),
            }
        }
    }
}

/// Unit testing.
#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function for running test cases.
    /// Arguments are:
    ///  * `minlen`: minimum length of sequences in the FASTA file to be
    ///     considered by the program.
    ///  * `input`: a string containing the contents of the input FASTA file.
    ///  * `expected`: the expected result of computing statistics for the input
    ///     FASTA file. `None` means that we do not expect any statistics to be
    ///     computed because there are no sequences in the input which meet the
    ///     minimum length requirements. `Some(stats)` means that we expect some
    ///     particular statistics to be computed.
    /// We compute the `FastaStats` for the input FASTA file and then compare against
    /// the expected result. If they are different we `panic` with a error message.
    fn test_fastastats_ok(minlen: u64, input: &String, expected: Option<FastaStats>) -> () {
        match FastaStats::new(minlen, input.as_bytes()) {
            Ok(result) => {
                if result != expected {
                    panic!(format!("expected {:?} got {:?}", expected, result))
                }
            }
            Err(_error) => panic!(format!("expected {:?} got Err(..)", expected)),
        }
    }

    /// Test for computations which are expected to fail.
    /// NB io::Error does not currently implement PartialEq, so we resort to
    /// just checking for the existence of an error, not its actual content.
    /// Hopefully this will be fixed in Rust soon:
    /// https://github.com/rust-lang/rust/issues/34158
    fn test_fastastats_err(minlen: u64, input: &String) -> () {
        let comp = FastaStats::new(minlen, input.as_bytes());
        match comp {
            Ok(result) => panic!(format!("expected Err(..) got {:?}", result)),
            Err(_error) => (),
        }
    }

    /// Empty input FASTA file. Should not produce any statistics.
    #[test]
    fn zero_byte_input() {
        test_fastastats_ok(0, &String::from(""), None)
    }

    /// Input FASTA file consisting of just a newline character.
    /// Result is expected to be an error, as the input is not considered
    /// a valid FASTA file.
    #[test]
    fn single_newline_input() {
        test_fastastats_err(0, &String::from("\n"))
    }

    /// Input FASTA file consisting of a single greater than sign,
    /// which is the minimal requirement for the FASTA header.
    #[test]
    fn single_greater_than_input() {
        test_fastastats_ok(0,
                           &String::from(">"),
                           Some(FastaStats {
                               min_len: 0,
                               average_len: 0,
                               max_len: 0,
                               total: 0,
                               num_seqs: 1,
                           }))
    }

    /// Input FASTA file consisting of a single sequence.
    /// The sequence is split over two lines.
    #[test]
    fn one_sequence() {
        test_fastastats_ok(0,
                           &String::from(">header\nATGC\nA"),
                           Some(FastaStats {
                               min_len: 5,
                               average_len: 5,
                               max_len: 5,
                               total: 5,
                               num_seqs: 1,
                           }))
    }

    /// Input FASTA file consisting of two sequences.
    /// The sequences are split over multiple lines.
    #[test]
    fn two_sequence() {
        test_fastastats_ok(0,
                           &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
                           Some(FastaStats {
                               min_len: 2,
                               average_len: 4,
                               max_len: 7,
                               total: 9,
                               num_seqs: 2,
                           }))
    }

    /// Input FASTA file without a sequence header. This is considered an
    /// error, because it is not a valid FASTA file.
    #[test]
    fn no_header() {
        test_fastastats_err(0, &String::from("no header\n"))
    }

    /// Input FASTA file with 2 sequences, and minlen less than the lengths
    /// of all the sequences in the file. None of the sequences should be
    /// filtered out.
    #[test]
    fn minlen_less_than_all() {
        test_fastastats_ok(2,
                           &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
                           Some(FastaStats {
                               min_len: 2,
                               average_len: 4,
                               max_len: 7,
                               total: 9,
                               num_seqs: 2,
                           }))
    }

    /// Input FASTA file with 2 sequences, and minlen greater than
    /// the length of one of the sequences. This sequence should be
    /// filtered out, and not considered in the calculation of the
    /// statistics.
    #[test]
    fn minlen_greater_than_one() {
        test_fastastats_ok(3,
                           &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
                           Some(FastaStats {
                               min_len: 7,
                               average_len: 7,
                               max_len: 7,
                               total: 7,
                               num_seqs: 1,
                           }))
    }

    /// Input FASTA file with 2 sequences, and minlen greater than
    /// the length of all the sequences. All sequences should be filtered
    /// out, and thus there are no statistics to compute.
    #[test]
    fn minlen_greater_than_all() {
        test_fastastats_ok(8,
                           &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
                           None)
    }
}
