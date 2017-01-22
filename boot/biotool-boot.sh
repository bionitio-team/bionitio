#!/usr/bin/env bash

# 1. Parse command line arguments.
# 2. Try to create new directory for the project.
# 3. Clone biotool git repository into the newly created directory.
# 4. Recursively copy source tree from the git repository into the project directory.
# 5. Recursively copy test_data directory from the git repository into the project directory.
# 6. Remove the cloned git repository.
# 7. Rename biotool to the new project name.
# 8. Create repository for new project.

#set -x

program_name="biotool-boot.sh"
# The name of the programming language that the user wants to use from the
# available biotool implementations
language=""
# The name of the new software project that the user wants to create
new_project_name=""
# Verbose output
verbose=""
# Name of temporary sub-directory to store biotool git repository
git_tmp_dir="biotool-boot-git-tmp"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: initialise a new bioinformatics project, starting from biotool

Usage:
    ${program_name} [-h] [-v] -l language -n new_project_name

Example:
    ${program_name} -l python -n skynet

The above example Will try to initialise a new project in directory 'skynet'
based on the 'python' biotool implementation.

If a directory already exists in the current working directory with the same
name as the new_project_name then this installer will not continue.

Valid languages are:
c, clojure, cpp, haskell, java, js, perl5, python, r, ruby, rust

-h shows this help message

-v verbose output

UsageMessage
}

# Parse the command line arguments and set the global variables language and new_project_name
function parse_args {
    local OPTIND opt

    while getopts "hl:n:v" opt; do
        case "${opt}" in
            h)
                show_help
                exit 0
                ;;
            l)  language="${OPTARG}"
                ;;
            n)  new_project_name="${OPTARG}"
                ;;
	    v)  verbose=true
		;;
        esac
    done

    shift $((OPTIND-1))

    [ "$1" = "--" ] && shift

    if [[ -z ${language} ]]; then
        echo "${program_name}: ERROR: missing command line argument: -l language, use -h for help"
        exit 2
    fi

    case ${language} in
        c|clojure|cpp|haskell|java|js|perl5|python|r|ruby|rust)
            # this is an allowed language
            ;;
        *)
            echo "${program_name}: ERROR: ${language} is not one of the valid languages, use -h to see the list"
            exit 2
    esac

    if [[ -z ${new_project_name} ]]; then
        echo "${program_name}: ERROR: missing command line argument: -n new_project_name, use -h for help"
        exit 2
    fi
}

function create_project_directory {
    if [[ -d ${new_project_name} ]]; then
        echo "${program_name}: ERROR: directory ${new_project_name} already exists, try another name or location, or rename the existing directory"
        exit 1
    else
        mkdir ${new_project_name} || {
            echo "${program_name}: ERROR: failed to create directory ${new_project_name}"
            exit 1
        }
    fi
}

function clone_biotool_repository {
    # XXX check if git is executable, catch output from git in case we need to report an error
    git clone https://github.com/biotool-paper/biotool ${new_project_name}/${git_tmp_dir} > /dev/null 2>&1 || {
        echo ${program_name}: ERROR: git command failed: 'git clone https://github.com/biotool-paper/biotool ${new_project_name}/${git_tmp_dir}'
        exit 1
    }
}

function copy_biotool_language {
    cp -R ${new_project_name}/${git_tmp_dir}/${language}/* ${new_project_name} || {
        echo ${program_name}: ERROR: copy command failed: 'cp -R {new_project_name}/${git_tmp_dir}/$language/ ${new_project_name}'
        exit 1
    }
}

function copy_test_data {
    cp -R ${new_project_name}/${git_tmp_dir}/test_data/ ${new_project_name}/test_data/ || {
        echo ${program_name}: ERROR: copy command failed: 'cp -R ${new_project_name}/${git_tmp_dir}/test_data/ ${new_project_name}/test_data/'
        exit 1
    }
}

function remove_biotool_repository {
    /bin/rm -fr "${new_project_name}/${git_tmp_dir}"
}

function rename_project {
    # Annoyingly BSD sed and GNU sed differ in handling the -i option (update in place).
    # So we opt for a portable approach which creates backups of the original ending in .temporary,
    # which we then later delete. It is ugly, but it is portable.
    # See: http://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
    # BSD sed does not support case insensitive matching, so we have to repeat for each
    # style of capitalisation.
    
    # XXX need to rename directory and file names
    
    # project name with first character upper case
    first_upper_project_name="$(tr '[:lower:]' '[:upper:]' <<< ${new_project_name:0:1})${new_project_name:1}"
    
    # project name with all characters upper case
    all_upper_project_name="$(tr '[:lower:]' '[:upper:]' <<< ${new_project_name})"
    
    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary  "s/biotool/${new_project_name}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete
    
    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary  "s/Biotool/${first_upper_project_name}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete
    
    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary  "s/BIOTOOL/${all_upper_project_name}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete
}

function create_project_repository {
    (
        cd ${new_project_name}
        git init
        git add .
        git commit -m "Initial commit of ${new_project_name}; starting from biotool (${language})"
    ) > /dev/null 2>&1
}

function verbose_message {
    if [ "${verbose}" = true ]; then
        echo "${program_name} $1"
    fi
}

# 1. Parse command line arguments.
parse_args $@
# 2. Try to create new directory for the project.
verbose_message "creating project directory ${new_project_name}"
create_project_directory
# 3. Clone biotool git repository into the newly created directory.
verbose_message "cloning biotool repository into ${new_project_name}/${git_tmp_dir}"
clone_biotool_repository
# 4. Recursively copy source tree from the git repository into the project directory.
verbose_message "copying ${language} source tree into ${new_project_name}"
copy_biotool_language
# 5. Recursively copy test_data directory from the git repository into the project directory.
verbose_message "copying test_data into ${new_project_name}"
copy_test_data
# 6. remove the cloned git repository.
verbose_message "removing ${new_project_name}/${git_tmp_dir}"
remove_biotool_repository
# 7. Rename biotool to the new project name.
verbose_message "renaming references to biotool to new project name ${new_project_name}" 
rename_project
# 8. Create repository for new project.
verbose_message "initialising new git repository for ${new_project_name}"
create_project_repository
