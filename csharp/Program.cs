using System;
using Bio.Util;
using Serilog;
using Microsoft.Extensions.CommandLineUtils;
using Serilog.Core;

namespace csharp
{
    class Program
    {
        static Logger GetLogger(string filename = null)
        {
            var config = new LoggerConfiguration();
            if (filename != null)
                config.WriteTo.RollingFile(filename);
            var logger = config.CreateLogger();
            logger.Information("Program started");
            return logger;
        }

        static CommandLineApplication CreateCli(string[] args)
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
                // Default values
                var length = minlen.HasValue() ? long.Parse(minlen.Value()) : 0;

                // Setup logging
                var logger = GetLogger(log.Value());
                logger.Information($"Command line: {args.StringJoin(" ")}");

                // Print out the CSV line by line
                Console.WriteLine("FILENAME	NUMSEQ	TOTAL	MIN	AVG	MAX");
                try
                {
                    if (fastqs.Values.Count == 0)
                    {
                        logger.Information("Processing FASTA file from stdin");
                        var stats = FastaStats.Calculate(Console.OpenStandardInput(), length, "STDIN");
                        Console.WriteLine(stats);
                    }
                    else
                    {
                        foreach (var fasta in fastqs.Values)
                        {
                            logger.Information($"Processing FASTA file from {fasta}");
                            var stats = FastaStats.Calculate(fasta, length);
                            Console.WriteLine(stats);
                        }
                    }
                    return 0;
                }
                catch (IOException e)
                {
                    logger.Error(e.Message);
                    return 1;
                }
                catch (FastaException e)
                {
                    logger.Error(e.Message);
                    return 3;
                }
            });

            return cli;
        }

        static int Main(string[] args)
        {
            var cli = CreateCli(args);
            try
            {
                cli.Execute(args);
            }
            catch (CommandParsingException)
            {
                return 2;
            }
            return 0;
        }
    }
}