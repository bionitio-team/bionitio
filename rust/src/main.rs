extern crate bio;
extern crate argparse;
use std::io;
use std::cmp;
use bio::io::fasta;
use std::fmt;
use std::fs::File;
use argparse::{ArgumentParser, StoreTrue, Store, Print, Collect};

struct FastaStats {
   min_len: u64,
   average_len: u64,
   max_len: u64,
   total: u64,
   num_seqs: u64,
}

impl FastaStats {
   pub fn new<R: io::Read>(options: &Options, reader: R) -> Option<FastaStats> {
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
               if this_len >= options.minlen {
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
            // XXX handle errors properly
            Err(error) => println!("{}", error),
         }
      }
      if num_seqs > 0 {
         // XXX check whether integer division does floor
         let average_len = ((total as f64) / (num_seqs as f64)).floor() as u64;
         Some(FastaStats { min_len: min_len,
                           average_len: average_len, 
                           max_len: max_len, 
                           total: total, 
                           num_seqs: num_seqs })
      } 
      else {
         None
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
       ap.parse_args_or_exit();
   }
   return options;
}

fn compute_print_stats<R: io::Read>(options: &Options, filename: &String, reader: R) -> () {
   match FastaStats::new(options, reader) {
      Some(stats) => {
         println!("{}\t{}", filename, stats);
      },
      None => {
         println!("{}\t0\t0\t-\t-\t-", filename);
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
            Err(e) => {
               // XXX handle errors properly
               println!("{}", e);
            }
         }
      }
   }
}
