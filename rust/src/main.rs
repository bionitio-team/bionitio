extern crate bio;
use std::io;
use bio::io::fasta;

fn main() {
   let reader = fasta::Reader::new(io::stdin());
   for next in reader.records() {
      match next { 
         Ok(record) => {
            println!("{}", String::from_utf8_lossy(record.seq()));
         }, 
         Err(error) => println!("{}", error),
      }
   }
}
