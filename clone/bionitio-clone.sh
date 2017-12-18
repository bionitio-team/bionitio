#!/usr/bin/env bash

# Clone all the language-specific bionitio repositories from github

# 1. Parse command line arguments.
# 2. Check dependencies.
# 3. Try to clone each of the language specific bionitio repositories

program_name="bionitio-clone.sh"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: clone all the bionitio language-specific repositories 

Usage:
    ${program_name} [-h] [-v]

-h shows this help message

-v verbose output

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

# Parse the command line arguments 
function parse_args {
    local OPTIND opt

    while getopts "hv" opt; do
        case "${opt}" in
            h)
                show_help
                exit 0
                ;;
	    v)  verbose=true
		;;
        esac
    done

    shift $((OPTIND-1))

    [ "$1" = "--" ] && shift
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

function clone_repositories {
    languages='c clojure cpp csharp haskell java js perl5 python r ruby rust'
    for this_language in $languages; do
        verbose_message "cloning $this_language"
        clone_one_repository $this_language
    done
}

function clone_one_repository {
    CMD="git clone --recursive https://github.com/bionitio-team/bionitio-${1} > /dev/null 2>&1"
    echo $CMD
    run_command_exit_on_error $CMD
}

function verbose_message {
    if [ "${verbose}" = true ]; then
        echo "${program_name} $1"
    fi
}

# 1. Parse command line arguments.
parse_args $@
# 2. Check that dependencies are met
verbose_message "checking for dependencies"
check_dependencies
# 3. Try to clone each of the language specific bionitio repositories
clone_repositories
verbose_message "done"
