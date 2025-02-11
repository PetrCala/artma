#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

function print_help {
  cat <<EOF
Usage: ./bumpVersion.sh [<options>] <args>

Description: Bump the package to a new version

Args:
<semver-type> : The type of semver level to build. One of 'major', 'minor', 'patch'.

Options: 
--help | -h   : Print this help message.
EOF
}

if [[ $print_help == true ]] || [[ $1 == "help" ]] || [[ $# -ne 1 ]]; then
  print_help
  exit 0
fi

SEMVER_LEVEL=$1
ACCEPTED_SEMVER_LEVELS=("minor" "major" "patch")

if [[ ! "${ACCEPTED_SEMVER_LEVELS[*]}" =~ $SEMVER_LEVEL ]]; then
  error "Invalid semver level: $SEMVER_LEVEL. Must be one of $ACCEPTED_SEMVER_LEVELS."
  exit 1
fi

Rscript -e "
  source(\"$SCRIPTS_SRC_DIR/bump_version.R\")
  bump_version(semver_level=\"$SEMVER_LEVEL\")
"
