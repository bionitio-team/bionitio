extern crate bio;
extern crate argparse;
use std::io;
use std::io::Write;
use std::cmp;
use bio::io::fasta;
use std::fmt;
use std::fs::File;
use argparse::{ArgumentParser, StoreTrue, Store, Print, Collect};

const EXIT_FILE_IO_ERROR: i32 = 1;
const EXIT_COMMAND_LINE_ERROR: i32 = 2;
const EXIT_FASTA_PARSE_ERROR: i32 = 3;
static PROGRAM_NAME: &'static str = "biotool";

fn exit_with_error(status :i32, message: &String) -> () {
   writeln!(&mut std::io::stderr(), "{} ERROR: {}!", PROGRAM_NAME, message).unwrap();
   std::process::exit(status);
}

#[derive(Debug, PartialEq)]
pub struct FastaStats {
   min_len: u64,
   average_len: u64,
   max_len: u64,
   total: u64,
   num_seqs: u64,
}

#[derive(Debug)]
pub enum StatsResult {
   StatsNone,
   StatsSome(FastaStats),
   StatsError(io::Error)
}

impl FastaStats {
   pub fn new<R: io::Read>(minlen: u64, reader: R) -> Result<Option<FastaStats>, io::Error> {
      let fasta_reader = fasta::Reader::new(reader);
      let mut num_seqs:u64 = 0;
      let mut total:u64 = 0;
      let mut max_len:u64 = 0;
      let mut min_len:u64 = 0;
      let mut this_len:u64;

      for next in fasta_reader.records() {
         match next { 
            Ok(record) => {
               this_len = record.seq().len() as u64;
               if this_len >= minlen {
                  num_seqs += 1;
                  total += this_len; 
                  if num_seqs == 1 {
                     max_len = this_len;
                     min_len = this_len;
                  }
                  else {
                     max_len = cmp::max(max_len, this_len);
                     min_len = cmp::min(min_len, this_len);
                  }
               }
            }, 
            //Err(error) => return StatsResult::StatsError(error)
            Err(error) => return Err(error)
         }
      }
      if num_seqs > 0 {
         let average_len = ((total as f64) / (num_seqs as f64)).floor() as u64;
         Ok(Some(FastaStats { min_len: min_len,
            average_len: average_len, 
            max_len: max_len, 
            total: total, 
            num_seqs: num_seqs }))
      } 
      else {
         Ok(None)
      }
   }
}

impl fmt::Display for FastaStats {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{}\t{}\t{}\t{}\t{}", self.num_seqs, self.total,
         self.min_len, self.average_len, self.max_len)
   }
}

struct Options {
    verbose: bool,
    minlen: u64,
    fasta_files: Vec<String>,
}

fn parse_options() -> Options {
   let mut options = Options { verbose: false, minlen: 0, fasta_files: Vec::new() };
   {  let mut ap = ArgumentParser::new();
      ap.set_description("Print fasta stats");
      ap.refer(&mut options.verbose)
         .add_option(&["-v", "--verbose"], StoreTrue,
         "Print more stuff about what's happening");
      ap.refer(&mut options.minlen)
         .add_option(&["--minlen"], Store,
         "Minimum length sequence to include in stats");
      ap.refer(&mut options.fasta_files)
         .add_argument(&"fasta_files", Collect,
         "Input FASTA files");
      ap.add_option(&["--version"],
         Print(env!("CARGO_PKG_VERSION").to_string()), "Show version");
      match ap.parse_args() {
         Ok(()) => (),
         Err(0) => {
            // Help and version commands
            std::process::exit(0);
         },
         Err(_error_code) => {
            // Parse error
            std::process::exit(EXIT_COMMAND_LINE_ERROR);
         }
      }
   }
   return options
}

fn compute_print_stats<R: io::Read>(options: &Options, filename: &String, reader: R) -> () {
   match FastaStats::new(options.minlen, reader) {
      //StatsResult::StatsSome(stats) => {
      Ok(Some(stats)) => {
         println!("{}\t{}", filename, stats);
      },
      Ok(None) => {
         println!("{}\t0\t0\t-\t-\t-", filename);
      }
      Err(error) => {
         exit_with_error(EXIT_FASTA_PARSE_ERROR, &format!("{}", error))
      }
   }
}

fn main() {
   let options = parse_options();
   println!("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX");
   if options.fasta_files.len() == 0 {
      compute_print_stats(&options, &String::from("stdin"), io::stdin());
   }
   else {
      for filename in &options.fasta_files {
         match File::open(filename) {
            Ok(file) => {
               compute_print_stats(&options, filename, file);
            },
            Err(error) => {
               exit_with_error(EXIT_FILE_IO_ERROR, &format!("{}", error))
            }
         }
      }
   }
}

#[cfg(test)]
mod tests {
   use super::*;

   fn test_fastastats_ok(minlen: u64, input: &String, expected :Option<FastaStats>) -> () {
      match FastaStats::new(minlen, input.as_bytes()) {
         Ok(result) => {
            if result != expected {
               panic!(format!("expected {:?} got {:?}", expected, result))
            }
         },
         Err(_error) => {
            panic!(format!("expected {:?} got Err(..)", expected))
         }
      }
   }

   // io::Error does not currently implement PartialEq, so we resort to
   // just checking for the existence of an error, not its actual content.
   // Hopefully this will be fixed in Rust soon:
   // https://github.com/rust-lang/rust/issues/34158
   fn test_fastastats_err(minlen: u64, input: &String) -> () {
      let comp = FastaStats::new(minlen, input.as_bytes());
      match comp {
         Ok(result) => panic!(format!("expected Err(..) got {:?}", result)),
         Err(_error) => ()
      }
   }

   #[test]
   fn test_zero_byte_input() {
      test_fastastats_ok(0, &String::from(""), None)
   }

   #[test]
   fn test_single_newline_input() {
      test_fastastats_err(0, &String::from("\n"))
   }

   #[test]
   fn test_single_greater_than_input() {
      test_fastastats_ok(0, &String::from(">"),
         Some(FastaStats{min_len :0, average_len :0, max_len :0, total :0, num_seqs :1}))
   }

   #[test]
   fn one_sequence() {
      test_fastastats_ok(0, &String::from(">header\nATGC\nA"),
         Some(FastaStats{min_len :5, average_len :5, max_len :5, total :5, num_seqs :1}))
   }

   #[test]
   fn two_sequence() {
      test_fastastats_ok(0, &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
         Some(FastaStats{min_len :2, average_len :4, max_len :7, total :9, num_seqs :2}))
   }

   #[test]
   fn no_header() {
      test_fastastats_err(0, &String::from("no header\n"))
   }

   #[test]
   fn minlen_less_than_all() {
      test_fastastats_ok(2, &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
         Some(FastaStats{min_len :2, average_len :4, max_len :7, total :9, num_seqs :2}))
   }

   #[test]
   fn minlen_greater_than_one() {
      test_fastastats_ok(3, &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"),
         Some(FastaStats{min_len :7, average_len :7, max_len :7, total :7, num_seqs :1}))
   }

   #[test]
   fn minlen_greater_than_all() {
      test_fastastats_ok(8, &String::from(">header1\nATGC\nAGG\n>header2\nTT\n"), None)
   }
}
