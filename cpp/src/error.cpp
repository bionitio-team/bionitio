/*
File:       : error.cpp 
Description : Utilities for error reporting 
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : stable 
Portability : POSIX
*/

#include <cstdlib>
#include <iostream>
#include "error.h"
#include "constants.h"
#include "exit_status.h"

using namespace std;

/*
    Print an error message to stderr and exit the program.

    arguments:
        - message: the error message string, the program
              name and "ERROR" will be prepended onto this
              message. The message will be printed to
              stderr.
        - exit_status: the exit status to return when the
              program exits

    result:
        void
*/
void exit_with_error(string message, exit_status status)
{
    print_error(message);
    exit(status);
}

/*
    Print an error message to stderr.

    arguments:
        - message: the error message string, the program
              name and "ERROR" will be prepended onto this
              message. The message will be printed to
              stderr.
    result:
        void
*/
void print_error(string message)
{
    cerr << PROGRAM_NAME << " ERROR: " << message << endl;
}
