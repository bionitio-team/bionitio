
#include <stdarg.h>
#include <stdio.h>
#include <time.h>

#include "log.h"

/**
 * log a message to stderr if verbose is true
 */
void bionitio_log(FILE* log, const char *msg, ...) {
    if (log) {
        time_t current_time = time(NULL);
        char time_str[80];
        strftime(time_str, 80, "%Y-%m-%d %H:%M:%S", localtime(&current_time));
        char buffer[1024];
        va_list vl;
        va_start(vl, msg);
        vsnprintf(buffer, sizeof(buffer), msg, vl);
        fprintf(log, "%s: %s\n", time_str, buffer);
        va_end(vl);
    }
}
