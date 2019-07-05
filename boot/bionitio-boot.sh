#!/usr/bin/env bash

# Initialise a new bioinformatics project, starting from bionitio.

# 1. Parse command line arguments.
# 2. Check dependencies.
# 3. Check if the new project name is a valid name, and exit if it is invalid.
# 4. Check if the new directory already exists, and exit if it does.
# 5. Clone language specific bionitio git repository into a new directory.
# 6. Set the license for the project.
# 7. Remove unneeded contents such as .git and readme_includes directories/files.
# 8. Substitute placeholder variables in all files.
# 9. Rename bionitio to the new project name.
# 10. Create new git repository for new project.
# 11. Patch the README.md file to contain correct URLs and license information.
# 12. Optionally create new github remote and push to it.

#set -x

program_name="bionitio-boot.sh"
# The name of the programming language that the user wants to use from the
# available bionitio implementations
language=""
# The license used for the new project. It defaults to MIT.
license="MIT"
# The name of the new software project that the user wants to create
new_project_name=""
# Verbose output
verbose=""
# Github username, may be overwritten command line argument if provided by the user 
github_username="GITHUB_USERNAME"
# If true, attempt to create github remote repository
github_remote_required=false
# Optional author name
author_name="BIONITIO_AUTHOR"
# Optional author email 
author_email="BIONITIO_EMAIL"
# name of log file 
logfile="/dev/null"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: initialise a new bioinformatics project, starting from bionitio

Usage:
    ${program_name} [-h] [-v] [-c license] [-g github-username] [-a author-name] [-e author_email] [-l logfile] -i language -n new_project_name

Example:
    ${program_name} -c BSD-3-Clause -i python -n skynet

The above example will try to initialise a new project in directory 'skynet'
based on the 'python' bionitio implementation, using the BSD-3-Clause license.

-n choose a name for your new project
   If a directory already exists in the current working directory with the same
   name as the new_project_name then this installer will not continue.

-i choose a language implementation for your new project
   Valid languages are:
   c, clojure, cpp, csharp, haskell, java, js, perl5, python, r, ruby, rust

-c choose a license for your new project
   Valid licenses are:
   Apache-2.0, BSD-2-Clause, BSD-3-Clause, GPL-2.0, GPL-3.0, MIT
   If no license is supplied, it will default to using the MIT license.

-h shows this help message

-v verbose output, show more information about what this script is doing

-g if you specify a valid github username, a new remote repository will
   be created on github with new_project_name as the name. This assumes
   that you do not already have a repository with this name on github
   under the specified username. 

-a if you specify an author name, we will replace all occurrences of
   BIONITIO_AUTHOR with this name in all files in the new project.

-e if you specify an author email, we will replace all occurrences of
   BIONITIO_EMAIL with this email address in all files in the new project.

-l write log messages to the specified filename. This can be useful for
   debugging or monitoring the behaviour of this script. If this option
   is not specified, log messages will be discarded.

Dependencies:

   The following tools must be installed on your computer to use this script,
   and be accessible via the PATH environment variable:
   - bash
   - git 
   - curl 

UsageMessage
}


# echo an error message $1 and exit with status $2
function exit_with_error {
    error_message=$1
    exit_status_value=$2
    printf "${program_name}: ERROR: ${error_message}\n"
    exit ${exit_status_value}
}


# Parse the command line arguments and set the global variables language and new_project_name
function parse_args {
    local OPTIND opt

    while getopts "hc:i:l:n:g:a:e:v" opt; do
        case "${opt}" in
            h)
                show_help
                exit 0
                ;;
            i)  language="${OPTARG}"
                ;;
            l)  logfile="${OPTARG}"
                ;;
            c)  license="${OPTARG}"
                ;;
            n)  new_project_name="${OPTARG}"
                ;;
            g)  github_username="${OPTARG}"
                github_remote_required=true
                ;;
            a)  author_name="${OPTARG}"
                ;;
            e)  author_email="${OPTARG}"
                ;;
	    v)  verbose=true
		;;
        esac
    done

    shift $((OPTIND-1))

    [ "$1" = "--" ] && shift

    if [[ -z ${language} ]]; then
		exit_with_error "missing command line argument: -i language, use -h for help" 2
    fi

    case ${language} in
        c|clojure|cpp|csharp|haskell|java|js|perl5|python|r|ruby|rust)
            # this is an allowed language
            ;;
        *)
            exit_with_error "${language} is not one of the valid languages, use -h to see the list" 2
    esac

    case ${license} in
	Apache-2.0|BSD-2-Clause|BSD-3-Clause|GPL-2.0|GPL-3.0|MIT)
            # this is an allowed license 
            ;;
        *)
            exit_with_error "${license} is not one of the valid licenses, use -h to see the list" 2
    esac

    if [[ -z ${new_project_name} ]]; then
        exit_with_error "missing command line argument: -n new_project_name, use -h for help" 2
    fi

    # Connect file descriptor 3 to the logfile
    exec 3<>${logfile}
}


function check_dependencies {
    # Check for git
    git --version > /dev/null || {
       exit_with_error "git is not installed in the PATH\nPlease install git, and ensure it can be found in your PATH variable." 1
    }
    # Check for curl 
    curl --version > /dev/null || {
       exit_with_error "curl is not installed in the PATH\nPlease install curl, and ensure it can be found in your PATH variable." 1
    }
}


function check_project_name {
    case ${language} in
    r)
        if [[ ! ${new_project_name} =~ ^[A-Za-z]+[A-Za-z0-9.]*[A-Za-z0-9]$ ]]; then
            exit_with_error "${new_project_name} is an invalid name for an R project. The project name must start with a letter and can only contain letters, numbers and periods." 1
        fi
        ;;
    *)
        ;;
    esac
}


INTERACTIVE_SLEEP_RETRY_SECS=10
INTERACTIVE_NUM_RETRIES=3
# Execute a shell command 
# Arguments:
#   1) The command to run as a string 
#   2) A unique name identifying the command 
#   3) A string description of what it is doing, for verbose output
#   4) An optional directory to run the command in 
function run_command_interactive {
    command_to_run=$1
    command_description=$2
    verbose_message "$command_description"
    # execute the command and get its exit status
    verbose_message "executing command: ${command_to_run}"
    eval_exit_status=1
    num_tries_remaining=3
    while (( eval_exit_status != 0 && num_tries_remaining > 0 )); do
        eval "$command_to_run" >&3
        eval_exit_status=$?
	num_tries_remaining=$((num_tries_remaining - 1))
	if (( eval_exit_status != 0 && num_tries_remaining > 0 )); then
            echo "$command_description failed, trying again in $INTERACTIVE_SLEEP_RETRY_SECS seconds, $num_tries_remaining tries remaining"
            sleep $INTERACTIVE_SLEEP_RETRY_SECS
        fi	
    done

    if (( eval_exit_status != 0 && num_retries_remaining == 0 )); then
        echo "$command_description failed the maximum number of retries allowed ($INTERACTIVE_NUM_RETRIES)" 
        exit_with_error "${command_to_run}, failed with exit status ${eval_exit_status}" 1 
    fi
}


# Execute a shell command 
# Arguments:
#   1) The command to run as a string 
#   2) A unique name identifying the command 
#   3) A string description of what it is doing, for verbose output
#   4) An optional directory to run the command in 
function run_command {
    command_to_run=$1
    command_description=$2
    verbose_message "$command_description"
    # execute the command and get its exit status
    verbose_message "executing command: ${command_to_run}"
    eval "$command_to_run" 2>&3 >&3
    eval_exit_status=$?
    if [ ${eval_exit_status} -ne 0 ]; then
        exit_with_error "${command_to_run}, failed with exit status ${eval_exit_status}" 1
    fi
}


# Check if the directory for the new project already exists.
function check_if_directory_exists {
    if [[ -d ${new_project_name} ]]; then
        exit_with_error "directory ${new_project_name} already exists.\nTry another name or location, or rename/remove the existing directory." 1
    fi
}


function clone_repository {
    CMD="git clone --recursive https://github.com/bionitio-team/bionitio-${language} ${new_project_name}"
    run_command "$CMD" "clone bionitio repository"
}


# Remove the old git sub-directories
function remove_unneeded_contents {
    RM_COMMAND="/bin/rm -fr ${new_project_name}/.git ${new_project_name}/.gitmodules ${new_project_name}/functional_tests/.git ${new_project_name}/readme_includes/"
    run_command "$RM_COMMAND" "Removing unnecssary files from new repository" 
}


# Copy the user-specified (or default) license file from github into the repository
function set_license {
    CMD="curl --fail -s https://raw.githubusercontent.com/bionitio-team/bionitio/master/license_options/${license} > ${new_project_name}/LICENSE"
    run_command "$CMD" "Copy license file into repository"
}


# Replace special placeholder variables in files.
# These allow us to substitute special values, such as the name of the author
# into files in the new repository.
# Variables replaced:
#    BIONITIO_AUTHOR
#    BIONITIO_DATE
#    BIONITIO_EMAIL
#    BIONITIO_LICENSE
#    BIONITIO_GITHUB_USERNAME
# The command to perform the substitution makes temporary files, which must be deleted once complete
function substitute_placeholders {
    date_string=$(date "+%d %b %Y")
    FIND_REPLACE_CMD="find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary -e \"s/BIONITIO_AUTHOR/${author_name}/g\" -e \"s/BIONITIO_DATE/${date_string}/g\" -e \"s/BIONITIO_EMAIL/${author_email}/g\" -e \"s/BIONITIO_LICENSE/${license}/g\" -e \"s/BIONITIO_GITHUB_USERNAME/${github_username}/g\""
    FIND_DELETE_TEMPORARY_FILES_CMD="find ${new_project_name} -name \"*.temporary\" -type f -delete"
    run_command "${FIND_REPLACE_CMD} && ${FIND_DELETE_TEMPORARY_FILES_CMD}" "Substituting placeholder variables in files"
}


# Replace "bionitio" and various capitalisations with the name of the new project
# within all file contents and also in file and directory names.
function rename_project {
    # Annoyingly BSD sed and GNU sed differ in handling the -i option (update in place).
    # So we opt for a portable approach which creates backups of the original ending in .temporary,
    # which we then later delete. It is ugly, but it is portable.
    # See: http://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
    # BSD sed does not support case insensitive matching, so we have to repeat for each
    # style of capitalisation.

    # project name with first character upper case
    first_upper_project_name="$(tr '[:lower:]' '[:upper:]' <<< ${new_project_name:0:1})${new_project_name:1}"

    # project name with all characters upper case
    all_upper_project_name="$(tr '[:lower:]' '[:upper:]' <<< ${new_project_name})"

    # substitute all occurrences of bionitio in content
    REPLACE_IN_FILES_CMD="find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary -e \"s/bionitio/${new_project_name}/g\" -e \"s/Bionitio/${first_upper_project_name}/g\" -e \"s/BIONITIO/${all_upper_project_name}/g\""
    REMOVE_RENAME_TEMPORARY_FILES_CMD="find ${new_project_name} -name \"*.temporary\" -type f -delete"
    REPLACE_IN_FILES_REMOVE_TEMP_FILES_CMD="${REPLACE_IN_FILES_CMD} && ${REMOVE_RENAME_TEMPORARY_FILES_CMD}"
    run_command "${REPLACE_IN_FILES_REMOVE_TEMP_FILES_CMD}" "Replace bionitio with ${new_project_name} in all file contents"

    # rename directories and files
    # sort in reverse order so that we always move a directory's contents before
    # moving that directory
    # Move into the project directory (new_project_name) and use `find .` so
    # that we don't substitute the word bionitio if it happens to occur in new_project_name
    verbose_message "replace bionitio with ${new_project_name} in all directory and file names"
    verbose_message "renaming files and directories..."
    cd ${new_project_name}
    for old in $(find . -iname "*bionitio*" | sort -r); do
      basename="$(basename ${old})"
      dirname="$(dirname ${old})"
      new1="${basename//bionitio/${new_project_name}}"
      new2="${new1//Bionitio/${first_upper_project_name}}"
      newbase="${new2//BIONITIO/${all_upper_project_name}}"
      new="${dirname}/${newbase}"
      if [ "$old" != "$new" ]; then
        verbose_message "$old -> $new"
        if [ -e "$new" ]; then
          exit_with_error "cannot rename directory \"$old\" to \"$new\": \"$new\" exists. Choose a different project name." 1
        fi
          mv -- "$old" "$new"
      fi
    done
    cd ..
}


function create_project_repository {
   ( cd ${new_project_name}
      INIT_CMD="git init"
      run_command "$INIT_CMD" "Initialise new git repository"
      ADD_CMD="git add ."
      run_command "$ADD_CMD" "Add code to new git repository"
      #COMMIT_CMD="git commit -m \"Initial commit of ${new_project_name}; starting from bionitio (${language})\""
      #run_command "$COMMIT_CMD" "Commit files to new git repository"
      git commit -m \"Initial commit of ${new_project_name}; starting from bionitio (${language})\"
    )
}

# If $github_remote_required is true (as a consequence of setting the command line argument -g)
# we attempt to make a new remote repository on github
# with the $github_username and $new_project_name
function github_remote {
      create_github_repo 
    ( cd ${new_project_name}
      git_remote_add_origin 
      git_push_origin_master 
      echo "Your new GitHub repository is: https://github.com/${github_username}/${new_project_name}"
    )
}

function git_push_origin_master {
    echo "If requested for a username and password, enter your GitHub username and password"
    PUSH_CMD="git push -u origin master"
    run_command_interactive "$PUSH_CMD" "Push new repository to GitHub remote"
}

function git_remote_add_origin {
    REMOTE_ADD_CMD="git remote add origin https://github.com/${github_username}/${new_project_name}.git"
    run_command "$REMOTE_ADD_CMD" "Add GitHub remote to new git repository"
}

function create_github_repo {
    verbose_message "Creating GitHub remote for user $github_username with repository name: $new_project_name"
    GITHUB_JSON="'{\"name\": \"$new_project_name\"}'"
    echo "If requested for a password, enter your GitHub password for username $github_username"
    CREATE_REPO_CMD="curl --fail -sS -u $github_username https://api.github.com/user/repos -d $GITHUB_JSON"
    run_command_interactive "$CREATE_REPO_CMD" "Create remote repository on GitHub"
}

# Replace incorrect references in the README.md file to the appropriate
# username, project name and license name
function patch_readme {
    original_file="${new_project_name}/README.md"
    new_file="${new_project_name}/README.md.bak"
    SED_CMD="sed -e \"s/${new_project_name}-team/${github_username}/g\" \
	-e \"s/${new_project_name}-${language}/${new_project_name}/g\" \
	-e \"s/\[.* License\]/\[${license} License\]/g\" \
         ${original_file} > ${new_file} \
         && mv ${new_file} ${original_file}"
    run_command "$SED_CMD" "Replace placeholders in README.md"
}

# If user specifies -v on the command line we print out extra information to stdout.
# We also write these messages to the log file
function verbose_message {
    message="$1"
    echo "${program_name}: ${message}" >&3 
    if [ "${verbose}" = true ]; then
        echo "${program_name}: $1"
    fi
}

function optionally_push_github {
    if [[ ${github_remote_required} = true ]]; then
        github_remote
    else
        verbose_message "Skipping GitHub remote creation, no GitHub username specified, see -g command line option"
    fi
}

# 1. Parse command line arguments.
parse_args "$@"
verbose_message "command line: ${program_name} $*"
# 2. Check that dependencies are met
verbose_message "checking dependencies" 
check_dependencies 
# 3. Check project name is valid
check_project_name
# 4. Try to create new directory for the project.
verbose_message "checking if ${new_project_name} already exists"
check_if_directory_exists
# 5. Clone bionitio git repository into the newly created directory.
clone_repository 
# 6. Set the license for the project
set_license 
# 7. Remove unneeded contents 
remove_unneeded_contents
# 8. Substitute placeholder variables in all files 
substitute_placeholders 
# 9. Rename bionitio to the new project name.
rename_project
# 10. Patch the README.md file to contain correct URLs and license information.
patch_readme 
# 11. Create new repository for new project.
create_project_repository
# 12. Optionally create and push to remote repostory on github
optionally_push_github

verbose_message "successfully completed"
