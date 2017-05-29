var fs = require('fs')
var through = require('through2')

var fasta = require('./fasta-parser')
/// /////////////////////////////////////////////////////////

exports.processFiles = processFiles

// Stream to process a fasta file for stats
function processFasta (file, minlen) {
  var bp = 0
  var n = 0
  var min, max
  var stream = through.obj(nextSeq, done)
  return stream

    // Filter, and collect stats on each sequence
  function nextSeq (data, enc, next) {
    var obj = JSON.parse(data.toString())
    var l = obj.seq.length
    if (l >= minlen) {
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
function processFiles (files, minlen, logger, cb) {
    // Done when we have no files left!
  if (files.length === 0) { return }
  var file = files.shift()

  logger.info('Processing FASTA file: %s with minlen %i', file, minlen)

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
      .pipe(processFasta(file, minlen))
      .on('data', function (stats) {
        cb(file, stats)
        processFiles(files, minlen, logger)
      })
}
