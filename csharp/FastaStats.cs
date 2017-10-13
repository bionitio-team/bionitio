using System.Collections.Generic;
using System.IO;
using System.Linq;
using Bio.IO.FastA;

namespace csharp
{
    public class FastaStats
    {
        public long numSeqs;
        public long numBases;
        public long minLength;
        public long maxLength;
        public double average;

        public FastaStats()
        {
        }

        public override string ToString()
        {
            return $@"Number of Sequences: {numSeqs}
Number of Bases: {numBases}
Minimum Sequence Length: {minLength}
Maximum Sequence Length: {maxLength}
Average Sequence Length: {average}";
        }

        public static FastaStats Calculate(string filename)
        {
            var fileStream = new FileStream(filename, FileMode.Open);
            return Calculate(fileStream);
        }

        public static FastaStats Calculate(FileStream stream)
        {
            var parser = new FastAParser();
            var sequences = parser.Parse(stream);
            return Calculate(sequences);
        }

        public static FastaStats Calculate(IEnumerable<Bio.ISequence> sequences)
        {
            return sequences
                .GroupBy(sequence => "")
                .Select(grouping => new FastaStats
                {
                    numSeqs = grouping.Count(),
                    numBases = grouping.Sum(sequence => sequence.Count),
                    minLength = grouping.Min(sequence => sequence.Count),
                    maxLength = grouping.Min(sequence => sequence.Count),
                    average = grouping.Average(sequence => sequence.Count)
                }).First();
        }
    }
}