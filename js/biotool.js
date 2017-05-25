#!/usr/bin/env node

/**
 */

var opts = require('commander')
var fs = require('fs')
var through = require('through2')
var winston = require('winston')

var fasta = require('./lib/fasta-parser')

// Override handling of unknown options - someone decided exit code 2 was better than the default 1
opts.unknownOption = function (flag) {
  console.error()
  console.error("  error: unknown option `%s'", flag)
  console.error()
  process.exit(2)
}

// Option parsing
opts
  .version('0.1.0')
  .usage('[options] contigs.fasta [contigs2.fasta ...]')
  .description('Print fasta stats')
  .option('-m, --minlen <n>', 'Minimum length sequence to include in stats (default=0)', parseInt, 0)
  .option('-l, --log <LOG_FILE>', 'record program progress in LOG_FILE')
  .parse(process.argv)

// Default to reading stdin if there are no files specified
if (opts.args.length === 0) { opts.args = ['/dev/stdin'] }

// Setup logging
var logger = new (winston.Logger)()
logger.info('Command line')
if (opts.log !== undefined) {
  logger.configure({
    transports: [
      new (winston.transports.File)({ filename: opts.log })
    ]
  })
}

logger.info('Command line: %s', process.argv.join(' '))

/// /////////////////////////////////////////////////////////

// Stream to process a fasta file for stats
function processFasta (file) {
  var bp = 0
  var n = 0
  var min, max
  var stream = through.obj(nextSeq, done)
  return stream

    // Filter, and collect stats on each sequence
  function nextSeq (data, enc, next) {
    var obj = JSON.parse(data.toString())
    var l = obj.seq.length
    if (l >= opts.minlen) {
      min = n === 0 ? l : Math.min(l, min)
      max = n === 0 ? l : Math.max(l, max)
      n += 1
      bp += l
    }
    next()
  }

  function done () {
    if (n > 0) { this.push([n, bp, min, Math.floor(bp / n), max]) } else { this.push([n, bp, '-', '-', '-']) }
    this.push(null)
  }
}

// Loop over all the files specified
function processFiles (files) {
    // Done when we have no files left!
  if (files.length === 0) { return }
  var file = files.shift()

  logger.info('Processing FASTA file : %s', file)

    // Read the file, pipe through the fasta parser, then through our filter, then output as appropriate
  fs.createReadStream(file)
      .on('error', function (err) {
        console.error('Error reading file', err.path)
        logger.error('Error reading file : %s', err.path)
        process.exit(1)
      })
      .pipe(fasta())
      .on('error', function (err, x) {
        console.error('Failed parsing of file', file, err)
        logger.error('Error reading file : %s', err.path)
        process.exit(3)
      })
      .pipe(processFasta(file))
      .on('data', function (stats) {
        console.log([file].concat(stats).join('\t'))
        processFiles(files)
      })
}

console.log('FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX')
processFiles(opts.args)
