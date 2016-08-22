
#include <stdio.h>

#include "log.h"

void biotool_log(int verbose, const char *msg) {
    if (verbose) {
        fprintf(stderr, "%s\n", msg);
    }
}
