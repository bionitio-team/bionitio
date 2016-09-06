
#include <stdio.h>

#include "log.h"

/**
 * log a message to stderr if verbose is true
 */
void biotool_log(int verbose, const char *msg) {
    if (verbose) {
        fprintf(stderr, "%s\n", msg);
    }
}
