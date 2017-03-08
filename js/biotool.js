#!/usr/bin/env node

/**
 */

var opts = require('commander');
var fs = require('fs');
var through = require('through2')
var fasta = require('./lib/fasta-parser');


// Helper function to verbose option
function increaseVerbosity(v, total) {
  return total + 1;
}

// Override handling of unknown options - someone decided exit code 2 was better than the default 1
opts.unknownOption = function(flag) {
    console.error();
    console.error("  error: unknown option `%s'", flag);
    console.error();
    process.exit(2);
}

// Option parsing
opts
  .version('1.0.0')
  .usage('[options] contigs.fasta [contigs2.fasta ...]')
  .description('Print fasta stats')
  .option('-m, --minlen <n>', 'Minimum length sequence to include in stats (default=0)', parseInt, 0)
  .option('-v, --verbose', "Print more stuff about what's happening", increaseVerbosity, 0)
  .parse(process.argv);

// Default to reading stdin if there are no files specified
if (opts.args == 0)
    opts.args = ["/dev/stdin"];



////////////////////////////////////////////////////////////

// Stream to process a fasta file for stats
function process_fasta(file) {
    var bp=0;
    var n=0;
    var min,max;
    var stream = through.obj(next_seq, done)
    return stream

    // Filter, and collect stats on each sequence
    function next_seq(data, enc, next) {
        obj = JSON.parse(data.toString());
        var l = obj.seq.length
        if (l>=opts.minlen) {
            if (opts.verbose>=2)
                console.error([file, obj.id, l].join("\t"))
            min = n==0 ? l : Math.min(l,min)
            max = n==0 ? l : Math.max(l,max)
            n += 1
            bp += l
        }
        next()
    }

    function done() {
        if (n>0)
            this.push([n, bp, min, Math.floor(bp/n), max]);
        else
            this.push([n, bp, '-', '-', '-']);
        this.push(null);
    }
}

// Loop over all the files specified
function process_files(files) {
    // Done when we have no files left!
    if (files.length==0)
        return
    var file = files.shift()

    if (opts.verbose>=1)
        console.error("Processing: "+file);

    // Read the file, pipe through the fasta parser, then through our filter, then output as appropriate
    fs.createReadStream(file)
      .on('error', function(err) {
          console.error("Error reading file", err.path);
          process.exit(1);
      })
      .pipe(fasta())
      .on('error', function(err, x) {
          console.error("Failed parsing of file", file, err);
          process.exit(3);
      })
      .pipe(process_fasta(file))
      .on('data', function(stats) {
           console.log([file].concat(stats).join("\t"))
           process_files(files)
       });
}

console.log("FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX");
process_files(opts.args)
