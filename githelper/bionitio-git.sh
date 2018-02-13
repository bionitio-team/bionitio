#!/usr/bin/env bash

# Convenience tool for running simple git commands on all bionitio 
# repositories.

# 1. Parse command line arguments.
# 2. Check dependencies.
# 3. Try to run the appropriate git command in each of the language specific bionitio repositories

program_name="bionitio-clone.sh"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: run simple git commands on all the bionitio language-specific repositories 

Usage:
    ${program_name} [-h] [-v] -c COMMAND [-m MESSAGE]

-h shows this help message

-v verbose output

-c COMMAND, where COMMAND is one of: clone, pull, commit, push

-m MESSAGE, use MESSAGE as commit log, only used with the commit command

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

# Flag to make git quiet, in verbose mode we turn this into the empty string
quiet="--quiet"
# Commit log message for "git commit"
message=""
# Git action to run, could be: clone, pull, commit, push
command=""
# The list of valid languages
languages='c clojure cpp csharp haskell java js perl5 python r ruby rust'

# Parse the command line arguments 
function parse_args {
    local OPTIND opt

    while getopts "hvc:m:" opt; do
        case "$opt" in
            h)
                show_help
                exit 0
                ;;
	    v)  verbose=true
                # Make git not quiet
                quiet=""
		;;
	    c)  command="${OPTARG}"
		;;
	    m)  message="${OPTARG}"
		;;
        esac
    done

    shift $((OPTIND-1))

    [ "$1" = "--" ] && shift

    if [[ -z ${command} ]]; then
        exit_with_error "missing command line argument: -c COMMAND, use -h for help" 2
    fi

    if [[ ${command} == commit ]]; then
        if [[ -z ${message} ]]; then
            exit_with_error "message is required when using the commit command, you must also supply -m MESSAGE, use -h for help" 2
        fi
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
    cmd="$@"
    verbose_message "Running command: $cmd"
    eval "$cmd"
    exit_status=$?
    if [ $exit_status -ne 0 ]; then
        exit_with_error "command failed: $cmd" 1
    fi
}

function commit_repositories {
    for this_language in $languages; do
        repo="bionitio-${this_language}"
        verbose_message "commiting ${repo}"
        commit_one_repository $repo
    done
}

function commit_one_repository {
    if [ -d $1 ]; then
        # check if there is something to commit
        if ! git -C $1 status | grep -q "nothing to commit"; then
            CMD="git -C $1 commit $quiet -am \"$message\""
            run_command_exit_on_error "$CMD"
        else
            verbose_message "Nothing to commit"
        fi
    else
        exit_with_error "directory $1 does not exist" 1
    fi
}

function push_repositories {
    for this_language in $languages; do
        repo="bionitio-${this_language}"
        verbose_message "pushing ${repo}"
        push_one_repository $repo
    done
}

function push_one_repository {
    if [ -d $1 ]; then
        CMD="git -C $1 push $quiet origin master"
        run_command_exit_on_error $CMD
    else
        exit_with_error "directory $1 does not exist" 1
    fi
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
        CMD="git -C $1 pull $quiet"
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
    CMD="git clone $quiet --recursive https://github.com/bionitio-team/${1}"
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
    elif [ "$command" == "commit" ]; then
        commit_repositories
    elif [ "$command" == "push" ]; then
        push_repositories
    fi
}

# 1. Parse command line arguments.
parse_args "$@"
# 2. Check that dependencies are met
verbose_message "checking for dependencies"
check_dependencies
# 3. Try to run the appropriate git command in each of the language specific bionitio repositories
verbose_message "performing command $command"
perform_command
verbose_message "done"
