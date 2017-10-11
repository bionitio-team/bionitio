#ifndef BIONITIO_ERROR_H
#define BIONITIO_ERROR_H

#include <string>
#include "exit_status.h"

void exit_with_error(std::string, exit_status);
void print_error(std::string);
#endif
