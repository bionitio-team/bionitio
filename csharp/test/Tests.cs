using System.IO;
using System.Text;
using bionitio;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace test
{
    [TestClass]
    public class Tests
    {
        private Stream StringToStream(string input)
        {
            return new MemoryStream(Encoding.Default.GetBytes(input));
        }

        [TestMethod]
        public void Empty()
        {
            var stats = FastaStats.Calculate(StringToStream(""));
            Assert.AreEqual(stats, new FastaStats
            {
                NumSeqs = 0,
                NumBases = 0
            });
        }

        [TestMethod]
        public void Newline()
        {
            Assert.ThrowsException<FastaException>(() =>
            {
                FastaStats.Calculate(StringToStream("\n"));
            });
        }

        [TestMethod]
        public void Arrow()
        {
            Assert.ThrowsException<FastaException>(() =>
            {
                FastaStats.Calculate(StringToStream(">"));
            });
        }

        [TestMethod]
        public void Single()
        {
            var stats = FastaStats.Calculate(StringToStream(">header\nATGC\nA"));
            Assert.AreEqual(stats, new FastaStats
            {
                NumSeqs = 1,
                NumBases = 5,
                MinLength = 5,
                Average = 5,
                MaxLength = 5
            });
        }

        [TestMethod]
        public void Double()
        {
            var stats = FastaStats.Calculate(StringToStream(">header1\nATGC\nAGG\n>header2\nTT\n"));
            Assert.AreEqual(stats, new FastaStats
            {
                NumSeqs = 2,
                NumBases = 9,
                MinLength = 2,
                Average = 4.5,
                MaxLength = 7
            });
        }

        [TestMethod]
        public void NoHeader()
        {
            Assert.ThrowsException<FastaException>(() =>
            {
                FastaStats.Calculate(StringToStream("no header\n"));
            });
        }

        [TestMethod]
        public void MinlenLessThanAll()
        {
            var stats = FastaStats.Calculate(StringToStream(">header1\nATGC\nAGG\n>header2\nTT\n"), 2);
            Assert.AreEqual(stats, new FastaStats
            {
                NumSeqs = 2,
                NumBases = 9,
                MinLength = 2,
                Average = 4.5,
                MaxLength = 7
            });
        }

        [TestMethod]
        public void MinlenGreaterThanOne()
        {
            var stats = FastaStats.Calculate(StringToStream(">header1\nATGC\nAGG\n>header2\nTT\n"), 3);
            Assert.AreEqual(stats, new FastaStats
            {
                NumSeqs = 1,
                NumBases = 7,
                MinLength = 7,
                Average = 7,
                MaxLength = 7
            });
        }

        [TestMethod]
        public void MinlenGreaterThanAll()
        {
            var stats = FastaStats.Calculate(StringToStream(">header1\nATGC\nAGG\n>header2\nTT\n"), 8);
            Assert.AreEqual(stats, new FastaStats
            {
                NumSeqs = 0,
                NumBases = 0
            });
        }
    }
}