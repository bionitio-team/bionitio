using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Bio;
using Bio.IO.FastA;

namespace csharp
{
    public class FastaException : System.IO.IOException
    {
        public FastaException(string filename, Exception inner)
            : base($"Error parsing fasta file {filename}", inner)
        {
        }
    }

    public class FastaStats
    {
        public string Filename { get; private set; } = "<UNKNOWN>";
        public long NumSeqs { get; private set; }
        public long NumBases { get; private set; }
        public long? MinLength { get; private set; }
        public long? MaxLength { get; private set; }
        public double? Average { get; private set; }

        /// <summary>
        /// Converts a nullable struct to a string for the sake of CSV printing
        /// </summary>
        /// <param name="nullable"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        private string NullableCsvString<T>(T? nullable) where T : struct
        {
            if (nullable.HasValue)
                return nullable.ToString();
            else
                return "-";
        }

        public override string ToString()
        {
            return
                $"{Filename}	{NumSeqs}	{NumBases}	{NullableCsvString(MinLength)}	{NullableCsvString(Average)}	{NullableCsvString(MaxLength)}";
        }

        /// <summary>
        /// Calculates a set of CSV stats from the file provided as a filename
        /// </summary>
        /// <param name="filename"></param>
        /// <param name="minlen">Minimum sequence length. Sequences smaller than this are ignored</param>
        /// <returns></returns>
        /// <exception cref="IOException"></exception>
        public static FastaStats Calculate(string filename, long minlen = 0)
        {
            var fileStream = new FileStream(filename, FileMode.Open);
            return Calculate(fileStream, minlen, filename);
        }

        /// <summary>
        /// Calculates a set of CSV stats from the file provided as a stream
        /// </summary>
        /// <param name="stream"></param>
        /// <param name="minlen">Minimum sequence length. Sequences smaller than this are ignored</param>
        /// <param name="filename"></param>
        /// <returns></returns>
        /// <exception cref="FastaException"></exception>
        public static FastaStats Calculate(Stream stream, long minlen = 0, string filename = "<UNKNOWN>")
        {
            var parser = new FastAParser();
            IEnumerable<ISequence> sequences;

            try
            {
                sequences = parser.Parse(stream);
            }
            catch (Exception e)
            {
                throw new FastaException(filename, e);
            }
            return Calculate(sequences, minlen, filename);
        }

        /// <summary>
        /// Calculates a set of CSV stats from the list of sequences provided
        /// </summary>
        /// <param name="sequences"></param>
        /// <param name="minlen">Minimum sequence length. Sequences smaller than this are ignored</param>
        /// <param name="filename"></param>
        /// <returns></returns>
        public static FastaStats Calculate(IEnumerable<ISequence> sequences, long minlen = 0,
            string filename = "<UNKNOWN>")
        {
            try
            {
                return sequences
                    .Where(sequence => sequence.Count > minlen)
                    .GroupBy(sequence => "")
                    .Select(grouping => new FastaStats
                    {
                        NumSeqs = grouping.Count(),
                        NumBases = grouping.Sum(sequence => sequence.Count),
                        MinLength = grouping.Min(sequence => sequence.Count),
                        MaxLength = grouping.Min(sequence => sequence.Count),
                        Average = grouping.Average(sequence => sequence.Count),
                        Filename = filename
                    })
                    .DefaultIfEmpty(new FastaStats {Filename = filename})
                    .First();
            }
            catch (Exception e)
            {
                throw new IOException(filename, e);
            }
        }
    }
}