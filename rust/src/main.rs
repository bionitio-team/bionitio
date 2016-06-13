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
// exit status 2 currently used by argparse on failure of command
// line argument parsing.
const EXIT_FASTA_PARSE_ERROR: i32 = 3;
static PROGRAM_NAME: &'static str = "biotool";

fn exit_with_error(status :i32, message: &String) -> () {
   writeln!(&mut std::io::stderr(), "{} ERROR: {}!", PROGRAM_NAME, message).unwrap();
   std::process::exit(status);
}

pub struct FastaStats {
   min_len: u64,
   average_len: u64,
   max_len: u64,
   total: u64,
   num_seqs: u64,
}

pub enum StatsResult {
   StatsNone,
   StatsSome(FastaStats),
   StatsError(io::Error)
}

impl FastaStats {
   pub fn new<R: io::Read>(minlen: u64, reader: R) -> StatsResult {
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
            Err(error) => return StatsResult::StatsError(error)
         }
      }
      if num_seqs > 0 {
         let average_len = ((total as f64) / (num_seqs as f64)).floor() as u64;
         StatsResult::StatsSome(FastaStats { min_len: min_len,
                           average_len: average_len, 
                           max_len: max_len, 
                           total: total, 
                           num_seqs: num_seqs })
      } 
      else {
         StatsResult::StatsNone
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
       // XXX ideally we should be able to use our own exit status here. Currently uses 2.
       ap.parse_args_or_exit();
   }
   return options;
}

fn compute_print_stats<R: io::Read>(options: &Options, filename: &String, reader: R) -> () {
   match FastaStats::new(options.minlen, reader) {
      StatsResult::StatsSome(stats) => {
         println!("{}\t{}", filename, stats);
      },
      StatsResult::StatsNone => {
         println!("{}\t0\t0\t-\t-\t-", filename);
      }
      StatsResult::StatsError(error) => {
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

   #[test]
   fn test_zero_byte_input() {
      match FastaStats::new(0, "".as_bytes()) {
         StatsResult::StatsSome(_stats) => panic!("Zero byte input should produce no stats"),
         StatsResult::StatsError(_error) => panic!("Zero byte input should produce no stats, not an error"),
         StatsResult::StatsNone => ()
      }
   }

   #[test]
   fn test_single_newline_input() {
      match FastaStats::new(0, "\n".as_bytes()) {
         StatsResult::StatsSome(_stats) => panic!("Single newline input should produce an error"),
         StatsResult::StatsNone => panic!("Single newline input should produce an error, not no stats"),
         StatsResult::StatsError(_error) => ()
      }
   }
}
