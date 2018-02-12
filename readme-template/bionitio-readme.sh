#!/usr/bin/env bash

# Generate a README.md file for a bionitio implementation based on a
# template, and a language name.

# 1. Parse command line arguments.

program_name="bionitio-readme.sh"

# Help message for using the program.
function show_help {
cat << UsageMessage

${program_name}: Generate a README.md file for a bionitio implementation based on a template 

Usage:
    ${program_name} [-h] -t TEMPLATE.md -l LANGUAGE -i INCLUDE_DIR

-h shows this help message

UsageMessage
}


# echo an error message $1 and exit with status $2
function exit_with_error {
    printf "${program_name}: ERROR: $1\n"
    exit $2
}

# Path of the input template file
template_filepath=""
# language used in the bionitio implementation 
language=""

# Parse the command line arguments 
function parse_args {
    local OPTIND opt

    while getopts "ht:l:i:" opt; do
        case "${opt}" in
            h)
                show_help
                exit 0
                ;;
            t)
                template_filepath="${OPTARG}"
                ;;

            l)
                language="${OPTARG}"
                ;;
            i)
                include_dir="${OPTARG}"
                ;;
        esac
    done

    shift $((OPTIND-1))

    if [[ -z ${language} ]]; then
        exit_with_error "missing command line argument: -l LANGUAGE, use -h for help" 2
    fi

    if [[ -z ${template_filepath} ]]; then
        exit_with_error "missing command line argument: -t TEMPLATE.md, use -h for help" 2
    fi

    if [[ -z ${include_dir} ]]; then
        exit_with_error "missing command line argument: -i INCLUDE_DIR, use -h for help" 2
    fi

}

# Read each line of the input file and:
#   1. find replace each occurrence of LANGUAGE with $language
#   2. Check if the line starts with #include, and if so replace the line with
#      the contents of the included file, if it exists.
function process_lines {
    while IFS= read -r line 
    do
        # replace all instances of LANGUAGE on the line with $language
        line=`echo "$line" | sed "s/LANGUAGE/$language/g"`
        # check if the line starts with #include "filepath"
        include_file=`echo "$line" | sed 's/#include[ ]*["]\([^"]*\)["]/\1/'`
        if [ "$line" != "$include_file" ]; then
            if [ -f "$include_dir/$include_file" ]; then
                cat "$include_dir/$include_file"
            else
                echo "ERROR: include file $include_dir/$include_file not found"
            fi 
        else
            echo "$line"
        fi 
    done < $1
}

# 1. Parse command line arguments.
parse_args $@
process_lines "$template_filepath"
