
#include <stdio.h>
#include <stdlib.h>

#include <getopt.h>

#include "fasta.h"
#include "log.h"
/* #include "kseq.h" */

const char *VERSION="0.1";

const int EXIT_OK=0;
const int EXIT_FILE_IO_ERROR=1;
const int EXIT_INVALID_COMMAND=2;
const int EXIT_FASTA_FILE_ERROR=3;

const int DEFAULT_MIN_LEN = 0;
const char *HEADER="FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX";

void printUsage() {
    puts("Synopsis:\n"
         "  Print fasta stats\n"
         "Usage:\n"
         "  biotool [options] contigs.fasta [another.fa ...]\n"
         "Options:\n"
         "  --help       Show this help\n"
         "  --version    Print version and exit\n"
         "  --verbose    Print more stuff about what's happening\n"
         "  --minlen N   Minimum length sequence to include in stats "
         "(default=0)");
}

void printVersion() {
    printf("biotool version %s\n", VERSION);
}

int processFiles(int verbose, int minlen, char **files, int fileCount) {
    verbose && fprintf(stderr, "processing %i files with minlen %i...\n", fileCount, minlen);
    puts(HEADER);
    if (fileCount == 0) {
        biotool_log(verbose, "reading stdin...");
        struct FastaStats result = processFasta(stdin, verbose, minlen);
        if (result.sequences == 0) {
            fprintf(stdout, "stdin\t0\t0\t-\t-\t-\n");
        }
        else {
            fprintf(stdout, "stdin\t%lu\t%lu\t%lu\t%.0f\t%lu\n", result.sequences, result.bases, result.min, result.average, result.max);
        }
        biotool_log(verbose, "reading stdin: done");
    }
    else {
        verbose && fprintf(stderr, "reading %i files...\n", fileCount);
        for (int current = 0; current < fileCount; current++) {
            verbose && fprintf(stderr, "reading %s...\n", files[current]);
            FILE *fh = fopen(files[current], "r");
            if (!fh) {
                fprintf(stderr, "Failed to open '%s'\n", files[current]);
                return EXIT_FILE_IO_ERROR;
            }
            struct FastaStats result = processFasta(fh, verbose, minlen);
            if (result.sequences == 0) {
                fprintf(stdout, "%s\t0\t0\t-\t-\t-\n", files[current]);
            }
            else {
                fprintf(stdout, "%s\t%lu\t%lu\t%lu\t%.0f\t%lu\n", files[current], result.sequences, result.bases, result.min, result.average, result.max);
            }
            verbose && fprintf(stderr, "reading %s: done\n", files[current]);
        }
        verbose && fprintf(stderr, "reading %i files: done\n", fileCount);
    }
    return EXIT_OK;
}

int main(int argc, char** argv) {
    // parse arguments
    int c;
    static int flag = 0;
    static int verbose = 0;
    int minlen = DEFAULT_MIN_LEN;

    while (1) {
        static struct option long_options[] = {
            /* These options set a flag. */
            {"verbose", no_argument, &verbose, 1},
            {"version", no_argument, &flag, 1},
            {"help", no_argument,    &flag, 1},
            {"minlen",  required_argument, 0},
            {0, 0, 0, 0}
        };
  
        int option_index = 0;
        c = getopt_long(argc, argv, "vwhm:", long_options, &option_index);
  
        /* Detect the end of the options. */
        if (c == -1) {
          break;
        }
  
        switch (c) {
            case 0:
                /* verbose and version flags */
                if (long_options[option_index].flag != 0) {
                    if (option_index == 0) { // verbose
                    }
                    else if (option_index == 1) { // version
                        printVersion();
                        exit(EXIT_OK);
                    }
                    else if (option_index == 2) { // help
                        printUsage();
                        exit(EXIT_OK);
                    }
                }
                if (option_index == 3) { // minlen
                    //printf("got minlen %s\n", optarg);
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

    /* Print any remaining command line arguments (not options). */
    // printf ("optind %i argc %i\n", optind, argc);
    // if (optind < argc) {
    //     printf ("non-option ARGV-elements: ");
    //     while (optind < argc) {
    //         printf ("%s ", argv[optind++]);
    //     }
    //     putchar ('\n');
    // }

    int result = processFiles(verbose, minlen, &argv[optind], argc - optind);

    exit(result);
    // int l;
    // if (argc == 1) {  
    //     fprintf(stderr, "Usage: %s <in.seq>\n", argv[0]);  
    //     return 1;  
    // }
    // FILE *fp = fopen(argv[1], "r"); // STEP 2: open the file handler  
    // kseq_t *seq = kseq_init(fileno(fp)); // STEP 3: initialize seq  
    // while ((l = kseq_read(seq)) >= 0) { // STEP 4: read sequence  
    //     printf("name: %s\n", seq->name.s);  
    //     if (seq->comment.l) printf("comment: %s\n", seq->comment.s);  
    //     printf("seq: %s\n", seq->seq.s);  
    //     if (seq->qual.l) printf("qual: %s\n", seq->qual.s);  
    // }  
    // printf("return value: %d\n", l);  
    // kseq_destroy(seq); // STEP 5: destroy seq  
    // fclose(fp); // STEP 6: close the file handler  
}
