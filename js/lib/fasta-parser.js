// # fasta-parser
// > Buffer Stream parser from FASTA to JSON.
// >
// > doi: [?](?)
// > author: [Bruno Vieira](http://bmpvieira.com)
// > email: <mail@bmpvieira.com>
// > license: [MIT](https://raw.githubusercontent.com/bionode/fasta-parser/master/LICENSE)
//
// ---
//
// ## Usage
//
//     $ npm install fasta-parser
//
//     var parser = require('fasta-parser')
//
//     var fastaData = new Buffer ('>sequence1\n\
//     ATGCACGTCACGTCAGTACTCGTCAGTAC\n\
//     >sequence2\n\
//     CAGTCCTACTGCATGCATGCATGCATGCATCGATGCATGTCGACTGCATGCATGC\n')
//
//     var parser = fasta()
//     parser.on('data', function(data) {
//       console.log(JSON.parse(data.toString()))
//     })
//     parser.write(fastaData)
//     parser.end()
//     //   { id: 'sequence1',
//     //     seq: 'ATGCACGTCACGTCAGTACTCGTCAGTAC' }
//     //   { id: 'sequence2',
//     //     seq: 'CAGTCCTACTGCATGCATGCATGCATGCATCGATGCATGTCGACTGCATGCATGC' }
//
// For a more useful API, check the dependent module:
//
// [bionode-fasta](http://github.com/bionode/bionode-fasta)

var through = require('through2')
var split = require('split')
var pumpify = require('pumpify')
var BufferList = require('bl')

module.exports = function() {
  return pumpify(split(), parser())
}

function parser() {
  var cacheBuf
  var openID = new Buffer('{"id":"')
  var closeIDOpenSeq = new Buffer('","seq":"')
  var closeSeq = new Buffer('"}\n')
  var stream = through(transform, flush)

  return stream

  function transform(buf, enc, next) {
    if (buf[0] === 62) { // If line starts with '>', this is an ID
      if (cacheBuf) { // If a previous object is in cache, push it
        cacheBuf.append(closeSeq)
        this.push(cacheBuf.slice())
      }
      var id = buf.toString().slice(1).trim().replace(/"/g, '\\"')
      cacheBuf = new BufferList()
      cacheBuf.append(openID)
      cacheBuf.append(id)
      cacheBuf.append(closeIDOpenSeq)
    }
    else {
      cacheBuf.append(buf)
    }
    next()
  }

  function flush() {
    cacheBuf.append(closeSeq)
    this.push(cacheBuf.slice())
    this.push(null)
  }
}
