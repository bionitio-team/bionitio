extern crate bio;
use std::io;
use std::cmp;
use bio::io::fasta;

fn main() {
   let reader = fasta::Reader::new(io::stdin());
   let mut num_seqs = 0;
   let mut total = 0;
   let mut max_len = 0;
   let mut min_len = 0;
   let mut this_len;
   let mut first_seq = true;

   println!("FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX");

   for next in reader.records() {
      match next { 
         Ok(record) => {
            num_seqs += 1;
            this_len = record.seq().len();
            total += this_len; 
            max_len = cmp::max(max_len, this_len);
            if first_seq {
               min_len = this_len;
               first_seq = false;
            }
            else {
               min_len = cmp::min(min_len, this_len);
            }
         }, 
         Err(error) => println!("{}", error),
      }
   }
   if num_seqs > 0 {
      let average = ((total as f64) / (num_seqs as f64)).floor() as usize;
      println!("{}\t{}\t{}\t{}\t{}", num_seqs, total, min_len, average, max_len);
   }
   else {
      println!("0\t0\t-\t-\t-");
   }
}
