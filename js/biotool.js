#!/usr/bin/env node

/**
 */

var opts = require('commander');
var fasta = require('bionode-fasta');
var fs = require('fs');
var through = require('through2')

function increaseVerbosity(v, total) {
  return total + 1;
}

opts
  .version('1.0.0')
  .usage('[options] contigs.fasta [contigs2.fasta ...]')
  .description('Print fasta stats')
  .option('-m, --minlen <n>', 'Minimum length sequence to include in stats (default=0)', parseInt, 0)
  .option('-v, --verbose', "Print more stuff about what's happening", increaseVerbosity, 0)
  .parse(process.argv);

if (opts.args == 0)
    opts.args = ["/dev/stdin"];

// Stream to process a fasta file for stats
function process_fasta(file) {
    var bp=0;
    var n=0;
    var min,max;
    var stream = through.obj(next_seq, done)
    return stream

    function next_seq(data, enc, next) {
        var l = data.seq.length
        if (l>=opts.minlen) {
            if (opts.verbose>=2)
                console.error([file, data.id, l].join("\t"))
            min = n==0 ? l : Math.min(l,min)
            max = n==0 ? l : Math.max(l,max)
            n += 1
            bp += l
        }
        next()
    }

    function done() {
        this.push([n, bp, min, Math.floor(bp/n), max]);
        this.push(null);
    }
}

function process_files(files) {
    if (files.length==0)
        return
    var file = files.shift()
    if (opts.verbose>=1)
        console.error("Processing: "+file);
    fs.createReadStream(file)
      .pipe(fasta({objectMode:true}))
      .pipe(process_fasta(file))
      .on('data', function(stats) {
           console.log([file].concat(stats).join("\t"))
           process_files(files)
       });
}

process_files(opts.args)

