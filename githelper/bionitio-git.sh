#!/usr/bin/env bash

# Convenience tool for cloning or bringing all bionitio  
# repositories up-to-date. 

# 1. Parse command line arguments.
# 2. Check dependencies.
# 3. Try to clone each of the language specific bionitio repositories

program_name="bionitio-clone.sh"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: clone all the bionitio language-specific repositories 

Usage:
    ${program_name} [-h] [-v] -c COMMAND

-h shows this help message

-v verbose output

-c COMMAND, where COMMAND is one of clone, pull

Dependencies:

   The following tools must be installed on your computer to use this script,
   and be accessible via the PATH environment variable:
   - git 

UsageMessage
}


# echo an error message $1 and exit with status $2
function exit_with_error {
    printf "${program_name}: ERROR: $1\n"
    exit $2
}

command=""
# The list of valid languages
languages='c clojure cpp csharp haskell java js perl5 python r ruby rust'

# Parse the command line arguments 
function parse_args {
    local OPTIND opt

    while getopts "hvc:" opt; do
        case "${opt}" in
            h)
                show_help
                exit 0
                ;;
	    v)  verbose=true
		;;
	    c)  command="${OPTARG}"
		;;
        esac
    done

    shift $((OPTIND-1))

    [ "$1" = "--" ] && shift

    if [[ -z ${command} ]]; then
                exit_with_error "missing command line argument: -c COMMAND, use -h for help" 2
    fi
}

function check_dependencies {
    # Check for git
    git --version > /dev/null || {
       exit_with_error "git is not installed in the PATH\nPlease install git, and ensure it can be found in your PATH variable." 1
    }
}

# Execute a string as a shell command and exit with an error if the command fails
function run_command_exit_on_error {
    eval "$@" || {
        exit_with_error "command failed: \'$@\'" 1
    }
}

function pull_repositories {
    for this_language in $languages; do
        repo="bionitio-${this_language}"
        verbose_message "pulling ${repo}"
        pull_one_repository $repo
    done
}

function pull_one_repository {
    if [ -d $1 ]; then
        CMD="git -C $1 pull > /dev/null 2>&1"
        run_command_exit_on_error $CMD
    else
        exit_with_error "directory $1 does not exist" 1
    fi
}

function clone_repositories {
    for this_language in $languages; do
        repo="bionitio-${this_language}"
        verbose_message "cloning ${repo}"
        clone_one_repository $repo
    done
}

function clone_one_repository {
    CMD="git clone --recursive https://github.com/bionitio-team/${1} > /dev/null 2>&1"
    run_command_exit_on_error $CMD
}

function verbose_message {
    if [ "${verbose}" = true ]; then
        echo "${program_name} $1"
    fi
}

function perform_command {
    if [ "$command" == "clone" ]; then
        clone_repositories
    elif [ "$command" == "pull" ]; then
        pull_repositories
    fi
}

# 1. Parse command line arguments.
parse_args $@
# 2. Check that dependencies are met
verbose_message "checking for dependencies"
check_dependencies
perform_command
verbose_message "done"
