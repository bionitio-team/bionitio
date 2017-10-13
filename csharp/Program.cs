using System;
using Microsoft.Extensions.CommandLineUtils;
using Bio.IO.FastA;

namespace csharp
{
    class Program
    {
        static void Main(string[] args)
        {
            var cli = new CommandLineApplication
            {
                Name = "bionitio",
                Description = "Print fasta stats"
            };
            
            // Args
            cli.HelpOption("-?|-h|--help");
            var fastqs = cli.Argument("fastqs", "Input FASTA files", true);
            var minlen = cli.Option("--minlen", "Minimum length sequence to include in stats (default 0)",
                CommandOptionType.SingleValue);
            var log = cli.Option("--log", "record program progress in LOG_FILE", CommandOptionType.SingleValue);
            
            // Parsing
            cli.OnExecute(() =>
            {
                var parser = new FastAParser();
                foreach (var fasta in fastqs.Values)
                {
                    var stats = FastaStats.Calculate(fasta);
                    Console.WriteLine(stats);
                }
                return 0;
            });

            cli.Execute(args);
        }
    }
}
