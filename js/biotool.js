#!/usr/bin/env node

/**
 */

var opts = require('commander')
var winston = require('winston')

var biotool = require('./lib/fasta-stats')

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

console.log('FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX')
biotool.processFiles(opts.args, opts.minlen, logger, function (file, stats) {
  console.log([file].concat(stats).join('\t'))
})
