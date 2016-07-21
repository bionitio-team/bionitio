#include <cstdlib>
#include <iostream>
#include "error.h"
#include "constants.h"
#include "exit_status.h"

using namespace std;

void exit_with_error(const char *message, exit_status status)
{
    print_error(message);
    exit(status);
}

void print_error(const char *message)
{
    cerr << PROGRAM_NAME << " ERROR: " << message << endl;
}
