
#include <stdio.h>
#include <stdlib.h>

#include <getopt.h>

#include "fasta.h"
#include "log.h"

const char *VERSION="0.1";

const int EXIT_OK=0;
const int EXIT_FILE_IO_ERROR=1;
const int EXIT_INVALID_COMMAND=2;
const int EXIT_FASTA_FILE_ERROR=3;

const int DEFAULT_MIN_LEN = 0;
const char *HEADER="FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX";

/**
 * print usage information to stdout
 */
void printUsage() {
    puts("Synopsis:\n"
         "  Print fasta stats\n"
         "Usage:\n"
         "  bionitio [options] contigs.fasta [another.fa ...]\n"
         "Options:\n"
         "  --help         Show this help\n"
         "  --version      Print version and exit\n"
         "  --log LOG_FILE Log program progress to LOG_FILE\n"
         "  --minlen N     Minimum length sequence to include in stats "
         "(default=0)");
}

/**
 * print version information to stdout
 */
void printVersion() {
    printf("bionitio version %s\n", VERSION);
}

/**
 * process and print stats about each file specified in files
 * @param log file handle to write additional details details, or null
 * @param minlen only print statis of sequences that are at least this long
 * @param files list of files to process
 * @param fileCount how many files are there to progress?
 * @return 0 if no errors encountered
 */
int processFiles(FILE *logFile, int minlen, char **files, int fileCount) {
    bionitio_log(logFile, "bionitio version %s starting.", VERSION);
    bionitio_log(logFile, "processing %i file(s) with minlen %i...", fileCount, minlen);
    puts(HEADER);
    if (fileCount == 0) {
        bionitio_log(logFile, "reading stdin...");
        struct FastaStats result = processFasta(stdin, logFile, minlen);
        if (result.total_sequences == 0 && !result.is_empty) { // no sequences at all is an error
            fprintf(stderr, "Invalid fasta file format\n");
            return EXIT_FASTA_FILE_ERROR;
        }
        if (result.sequences == 0) {
            fprintf(stdout, "stdin\t0\t0\t-\t-\t-\n");
        }
        else {
            fprintf(stdout, "stdin\t%lu\t%lu\t%lu\t%.0f\t%lu\n", result.sequences, result.bases, result.min, result.average, result.max);
        }
        bionitio_log(logFile, "reading stdin: done");
    }
    else {
        bionitio_log(logFile, "reading %i file(s)...", fileCount);
        for (int current = 0; current < fileCount; current++) {
            bionitio_log(logFile, "reading %s...", files[current]);
            FILE *fh = fopen(files[current], "r");
            if (!fh) {
                fprintf(stderr, "Failed to open '%s'\n", files[current]);
                return EXIT_FILE_IO_ERROR;
            }
            struct FastaStats result = processFasta(fh, logFile, minlen);
            if (result.total_sequences == 0 && !result.is_empty) { // no sequences at all is an error
                fprintf(stderr, "Invalid fasta file format '%s'\n", files[current]);
                return EXIT_FASTA_FILE_ERROR;
            }
            if (result.sequences == 0) {
                fprintf(stdout, "%s\t0\t0\t-\t-\t-\n", files[current]);
            }
            else {
                fprintf(stdout, "%s\t%lu\t%lu\t%lu\t%.0f\t%lu\n", files[current], result.sequences, result.bases, result.min, result.average, result.max);
            }
            bionitio_log(logFile, "reading %s: done", files[current]);
        }
        bionitio_log(logFile, "reading %i file(s): done", fileCount);
    }
    return EXIT_OK;
}

/**
 * main entry point
 */
int main(int argc, char** argv) {
    // parse arguments
    int c;
    static int flag = 0;
    FILE *logFile = 0;
    int minlen = DEFAULT_MIN_LEN;

    while (1) {
        static struct option long_options[] = {
            {"log", required_argument, 0},
            {"version", no_argument, &flag, 1},
            {"help", no_argument,    &flag, 1},
            {"minlen",  required_argument, 0},
            {0, 0, 0, 0}
        };

        int option_index = 0;
        c = getopt_long(argc, argv, "l:vhm:", long_options, &option_index);

        /* Detect the end of the options. */
        if (c == -1) {
            break;
        }

        switch (c) {
        case 0:
            /* verbose and version flags */
            if (long_options[option_index].flag != 0) {
                if (option_index == 1) { // version
                    printVersion();
                    exit(EXIT_OK);
                }
                else if (option_index == 2) { // help
                    printUsage();
                    exit(EXIT_OK);
                }
            }
            if (option_index == 0) { // log
                logFile = fopen(optarg, "w");
            }
            else if (option_index == 3) { // minlen
                sscanf(optarg, "%i", &minlen);
            }
            break;

        case '?':
            /* getopt_long already printed an error message. */
            printUsage();
            exit(EXIT_INVALID_COMMAND);

        default:
            printUsage();
            exit(EXIT_INVALID_COMMAND);
        } // switch
    } // while

    int result = processFiles(logFile, minlen, &argv[optind], argc - optind);

    exit(result);
}
