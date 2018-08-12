#!/usr/bin/env bash

# Initialise a new bioinformatics project, starting from bionitio.

# 1. Parse command line arguments.
# 2. Check dependencies.
# 3. Check if the new directory already exists, and exit if it does. 
# 4. Clone language specific bionitio git repository into a new directory.
# 5. Set the license for the project.
# 6. Remove unneeded contents such as .git and readme_includes directories/files. 
# 7. Substitute placeholder variables in all files. 
# 8. Rename bionitio to the new project name.
# 9. Create new git repository for new project.
# 10. Patch the README.md file to contain correct URLs and license information.
# 11. Optionally create new github remote and push to it

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
# Optional github username
github_username=""
# Optional author name
author_name="BIONITIO_AUTHOR"
# Optional author email 
author_email="BIONITIO_EMAIL"
# Set this to empty string to make git verbose in output
git_quiet="--quiet"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: initialise a new bioinformatics project, starting from bionitio

Usage:
    ${program_name} [-h] [-v] [-c license] [-g github-username] [-a author-name] [-e author_email] -l language -n new_project_name

Example:
    ${program_name} -c BSD-3-Clause -l python -n skynet

The above example will try to initialise a new project in directory 'skynet'
based on the 'python' bionitio implementation, using the BSD-3-Clause license.

If a directory already exists in the current working directory with the same
name as the new_project_name then this installer will not continue.

If no license is supplied, it will default to using the MIT license.

Valid languages are:
c, clojure, cpp, csharp, haskell, java, js, perl5, python, r, ruby, rust

Valid licenses are:
Apache-2.0, BSD-2-Clause, BSD-3-Clause, GPL-2.0, GPL-3.0, MIT

These are just a selection of commonly used licenses, but you are free to
choose another, if you so desire.

-h shows this help message

-v verbose output

-g if you specify a valid github username, a new remote repository will
   be created on github with new_project_name as the name. This assumes
   that you do not already have a repository with this name on github
   under the specified username. 

-a if you specify an author name, we will replace all occurrences of
   BIONITIO_AUTHOR with this name in all files in the new project.

-e if you specify an author email, we will replace all occurrences of
   BIONITIO_EMAIL with this email address in all files in the new project.

Dependencies:

   The following tools must be installed on your computer to use this script,
   and be accessible via the PATH environment variable:
   - git 
   - curl 

UsageMessage
}


# echo an error message $1 and exit with status $2
function exit_with_error {
    printf "${program_name}: ERROR: $1\n"
    exit $2
}


# Parse the command line arguments and set the global variables language and new_project_name
function parse_args {
    local OPTIND opt

    while getopts "hc:l:n:g:a:e:v" opt; do
        case "${opt}" in
            h)
                show_help
                exit 0
                ;;
            l)  language="${OPTARG}"
                ;;
            c)  license="${OPTARG}"
                ;;
            n)  new_project_name="${OPTARG}"
                ;;
            g)  github_username="${OPTARG}"
                ;;
            a)  author_name="${OPTARG}"
                ;;
            e)  author_email="${OPTARG}"
                ;;
	    v)  verbose=true
		git_quiet=""
		;;
        esac
    done

    shift $((OPTIND-1))

    [ "$1" = "--" ] && shift

    if [[ -z ${language} ]]; then
		exit_with_error "missing command line argument: -l language, use -h for help" 2
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


# Execute a string as a shell command and exit with an error if the command fails
function run_command_exit_on_error {
    eval "$@"
    eval_exit_status=$?
    if [ $eval_exit_status != 0 ]; then
        exit_with_error "command failed: \'$@\' with exit code $eval_exit_status" 1
    fi 
}


function check_if_directory_exists {
    if [[ -d ${new_project_name} ]]; then
        exit_with_error "directory ${new_project_name} already exists, try another name or location, or rename the existing directory" 1
    fi
}


function clone_bionitio_repository {
    CMD="git clone $git_quiet --recursive https://github.com/bionitio-team/bionitio-${language} ${new_project_name}"
    run_command_exit_on_error $CMD
}


function remove_unneeded_contents {
    # Remove the old git sub-directories
    /bin/rm -fr ${new_project_name}/.git
    /bin/rm -f ${new_project_name}/.gitmodules
    /bin/rm -fr ${new_project_name}/functional_tests/.git
    /bin/rm -fr ${new_project_name}/readme_includes/
}


function set_license {
    CMD="curl -s https://raw.githubusercontent.com/bionitio-team/bionitio/master/license_options/${license} > ${new_project_name}/LICENSE"
    run_command_exit_on_error $CMD
}


function substitute_placeholders {
    date_string=$(date "+%d %b %Y")
    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary \
        -e "s/BIONITIO_AUTHOR/${author_name}/g" \
        -e "s/BIONITIO_DATE/${date_string}/g" \
        -e "s/BIONITIO_EMAIL/${author_email}/g" \
        -e "s/BIONITIO_LICENSE/${license}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete
}

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
    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary  "s/bionitio/${new_project_name}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete

    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary  "s/Bionitio/${first_upper_project_name}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete

    find ${new_project_name} -type f -print0 | xargs -0 sed -i.temporary  "s/BIONITIO/${all_upper_project_name}/g"
    find ${new_project_name} -name "*.temporary" -type f -delete

    # rename directories and files
    # sort in reverse order so that we always move a directory's contents before
    # moving that directory
    # Move into the project directory (new_project_name) and use `find .` so
    # that we don't substitute the word bionitio if it happens to occur in new_project_name
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
    INIT_CMD="git -C ${new_project_name} init $git_quiet"
    run_command_exit_on_error "$INIT_CMD"
    ADD_CMD="git -C ${new_project_name} add ."
    run_command_exit_on_error "$ADD_CMD"
    COMMIT_CMD="git -C ${new_project_name} commit $git_quiet -m \"Initial commit of ${new_project_name}; starting from bionitio (${language})\""
    run_command_exit_on_error "$COMMIT_CMD"
}

# If $github_username is defined (command line argument -g)
# we attempt to make a new remote repository on github
# with the $github_username and $new_project_name
function optional_github_remote {
    if [[ -z ${github_username} ]]; then
        verbose_message "Skipping github remote creation, no github username specified, see -g command line option"
    else
        verbose_message "Creating github remote for user $github_username with repository name: $new_project_name"
        GITHUB_JSON="'{\"name\": \"$new_project_name\"}'"
	echo "If requested, enter your github password for username $github_username"
        CREATE_REPO_CMD="curl -sS -u $github_username https://api.github.com/user/repos -d $GITHUB_JSON > /dev/null"
        run_command_exit_on_error "$CREATE_REPO_CMD"
	# Unfortunately "git remote add" and "git push" does not 
	# support the --quiet command line flag or are not totally quiet when it is given
	if [[ -z ${git_quiet} ]]; then
            REMOTE_ADD_CMD="git -C ${new_project_name} remote add origin https://github.com/${github_username}/${new_project_name}.git"
            PUSH_CMD="git -C ${new_project_name} push -u origin master"
	else
            REMOTE_ADD_CMD="git -C ${new_project_name} remote add origin https://github.com/${github_username}/${new_project_name}.git > /dev/null 2>&1"
            PUSH_CMD="git -C ${new_project_name} push -u origin master > /dev/null 2>&1"
	fi
        run_command_exit_on_error "$REMOTE_ADD_CMD"
        run_command_exit_on_error "$PUSH_CMD"
	echo "Your new github repository is: https://github.com/${github_username}/${new_project_name}"
    fi
}

# Replace incorrect references in the README.md file to the appropriate
# username, project name and license name
function patch_readme {
    username="USERNAME"
    if [ -n "$github_username" ]; then
        username="$github_username"
    fi
    original_file="${new_project_name}/README.md"
    new_file="${new_project_name}/README.md.bak"
    sed -e "s/${new_project_name}-team/${username}/g" \
	-e "s/${new_project_name}-${language}/${new_project_name}/g" \
	-e "s/\[.* License\]/\[${license} License\]/g" \
         ${original_file} > ${new_file} \
         && mv ${new_file} ${original_file}
}

function verbose_message {
    if [ "${verbose}" = true ]; then
        echo "${program_name} $1"
    fi
}


# 1. Parse command line arguments.
parse_args "$@"
# 2. Check that dependencies are met
verbose_message "checking for dependencies"
check_dependencies
# 3. Try to create new directory for the project.
verbose_message "Checking if ${new_project_name} already exists"
check_if_directory_exists
# 4. Clone bionitio git repository into the newly created directory.
verbose_message "cloning bionitio repository into ${new_project_name}"
clone_bionitio_repository
# 5. Set the license for the project
verbose_message "setting the license to ${license}"
set_license
# 6. Remove unneeded contents 
verbose_message "removing unneeded contents, such as git repository and readme_includes"
remove_unneeded_contents
# 7. Substitute placeholder variables in all files 
verbose_message "Substituting placeholder variables in all files"
substitute_placeholders
# 8. Rename bionitio to the new project name.
verbose_message "renaming references to bionitio to new project name ${new_project_name}" 
rename_project
# 9. Patch the README.md file to contain correct URLs and license information.
verbose_message "patching the README.md file"
patch_readme
# 10. Create new repository for new project.
verbose_message "initialising new git repository for ${new_project_name}"
create_project_repository
# 11. Optionally create and push to remote repostory on github
optional_github_remote
verbose_message "done"
