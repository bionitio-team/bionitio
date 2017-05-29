var assert = require('assert')
var tmp = require('tmp')
tmp.setGracefulCleanup()

describe('FastaParser', function () {
  describe('#parser()', function () {
    it('should call the data event for each sequence present in the fasta file', function (done) {
      var fasta = require('../lib/fasta-parser')
      var fastaData = Buffer.from('>sequence1\nATGCACGTCACGTCAGTACTCGTCAGTAC\n>sequence2\nCAGTCCTACTGCATGCATGCATGCATGCATCGATGCATGTCGACTGCATGCATGC\n')
      var parser = fasta()
      var count = 0
      parser.on('data', function (data) {
        count++
      })
      parser.on('end', function () {
        assert.equal(2, count)
        done()
      })
      parser.write(fastaData)
      parser.end()
    })
  })
})

describe('Biotool', function () {
  describe('#processFiles()', function () {
    it('should read a file and report statistics', function (done) {
      var tmpobj = tmp.fileSync()
      var biotool = require('../lib/fasta-stats')
      var fs = require('fs')
      var winston = require('winston')
      var logger = new (winston.Logger)()
      fs.writeSync(tmpobj.fd, '>s1\nATCG\n')
      fs.closeSync(tmpobj.fd)
      biotool.processFiles([tmpobj.name], 0, logger, function (file, stats) {
        assert.equal(tmpobj.name, file)
        assert.deepEqual([1, 4, 4, 4, 4], stats)
        done()
      })
    })
  })
})
