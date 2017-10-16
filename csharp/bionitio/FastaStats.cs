using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using Bio;
using Bio.IO.FastA;

[assembly: InternalsVisibleTo("test")]

namespace bionitio
{
    public class FastaException : System.IO.IOException
    {
        public FastaException(string filename, Exception inner)
            : base($"Error parsing fasta file {filename}", inner)
        {
        }
    }

    public class FastaStats : IEquatable<FastaStats>
    {
        public string Filename { get; internal set; } = "<UNKNOWN>";
        public long NumSeqs { get; internal set; }
        public long NumBases { get; internal set; }
        public long? MinLength { get; internal set; }
        public long? MaxLength { get; internal set; }
        public double? Average { get; internal set; }

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

        public override bool Equals(object obj)
        {
            if (obj is FastaStats)
                return Equals((FastaStats) obj);
            else
                return base.Equals(obj);
        }

        public bool Equals(FastaStats other)
        {
            return Filename == other.Filename &&
                   NumSeqs == other.NumSeqs &&
                   NumBases == other.NumBases &&
                   MinLength == other.MinLength &&
                   Average == other.Average &&
                   MaxLength == other.MaxLength;
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
                    .Where(sequence => sequence.Count >= minlen)
                    .GroupBy(sequence => "")
                    .Select(grouping => new FastaStats
                    {
                        NumSeqs = grouping.Count(),
                        NumBases = grouping.Sum(sequence => sequence.Count),
                        MinLength = grouping.Min(sequence => sequence.Count),
                        MaxLength = grouping.Max(sequence => sequence.Count),
                        Average = grouping.Average(sequence => sequence.Count),
                        Filename = filename
                    })
                    .DefaultIfEmpty(new FastaStats {Filename = filename})
                    .First();
            }
            catch (Exception e)
            {
                throw new FastaException(filename, e);
            }
        }
    }
}