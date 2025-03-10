#!/bin/bash

# Check if GREEN has already been defined
if [ -z "${GREEN+x}" ]; then
    declare -r GREEN=$'\e[1;32m'
fi

# Check if RED has already been defined
if [ -z "${RED+x}" ]; then
    declare -r RED=$'\e[1;31m'
fi

# Check if BLUE has already been defined
if [ -z "${BLUE+x}" ]; then
    declare -r BLUE=$'\e[1;34m'
fi

# Check if TITLE has already been defined
if [ -z "${TITLE+x}" ]; then
    declare -r TITLE=$'\e[1;4;34m'
fi

# Check if RESET has already been defined
if [ -z "${RESET+x}" ]; then
    declare -r RESET=$'\e[0m'
fi

function success {
    echo "🎉 $GREEN$1$RESET"
}

function error {
    echo "💥 $RED$1$RESET"
}

function info {
    echo "$BLUE$1$RESET"
}

function title {
    printf "\n%s%s%s\n" "$TITLE" "$1" "$RESET"
}

function assert_equal {
    if [[ "$1" != "$2" ]]; then
        error "Assertion failed: $1 is not equal to $2"
        exit 1
    else
        success "Assertion passed: $1 is equal to $1"
    fi
}

# Usage: join_by_string <delimiter> ...strings
# example: join_by_string ' + ' 'string 1' 'string 2'
# example: join_by_string ',' "${ARRAY_OF_STRINGS[@]}"
function join_by_string {
    local separator="$1"
    shift
    local first="$1"
    shift
    printf "%s" "$first" "${@/#/$separator}"
}

# Usage: get_abs_path <path>
# Will make a path absolute, resolving any relative paths
# example: get_abs_path "./foo/bar"
get_abs_path() {
    local the_path=$1
    local -a path_elements
    IFS='/' read -ra path_elements <<<"$the_path"

    # If the path is already absolute, start with an empty string.
    # We'll prepend the / later when reconstructing the path.
    if [[ "$the_path" = /* ]]; then
        abs_path=""
    else
        abs_path="$(pwd)"
    fi

    # Handle each path element
    for element in "${path_elements[@]}"; do
        if [ "$element" = "." ] || [ -z "$element" ]; then
            continue
        elif [ "$element" = ".." ]; then
            # Remove the last element from abs_path
            abs_path=$(dirname "$abs_path")
        else
            # Append element to the absolute path
            abs_path="${abs_path}/${element}"
        fi
    done

    # Remove any trailing '/'
    while [[ $abs_path == */ ]]; do
        abs_path=${abs_path%/}
    done

    # Special case for root
    [ -z "$abs_path" ] && abs_path="/"

    # Special case to remove any starting '//' when the input path was absolute
    abs_path=${abs_path/#\/\//\/}

    echo "$abs_path"
}

#' Execute an R script in a new R session
#' After the script is executed, the R session will be closed
run_r_script_interactively() {
    local script_full_path=$1

    # Check if the R script exists
    if [ ! -f "$script_full_path" ]; then
        error "R script not found: $script_full_path"
        exit 1
    fi

    R --interactive --no-save --quiet <<EOT
    source("$script_full_path")
EOT
}

# Function to extract and print metadata from a YAML file
extract_yaml_metadata() {
    local file="$1"
    local is_in_use="$2"

    if [[ ! -f "$file" ]]; then
        error "File not found: $file"
        return
    fi

    # Extract basic metadata
    local filename=$(basename "$file")
    local created=$(date -r "$file" "+%Y-%m-%d %H:%M:%S")
    local filesize=$(wc -c "$file" | awk '{print $1}')

    # Print the metadata in a single line
    local base_stats="File: $filename | Created: $created | File size: $filesize bytes"

    "$is_in_use" = true ]] && base_stats="$base_stats | In use"

    echo $base_stats
}
