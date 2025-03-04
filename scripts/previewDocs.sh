#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

function print_help {
  cat <<EOF
Usage: ./previewDocs.sh [<options>] <args>

Description: Open a documentation preview of an R object exported by this package.

Args:
<obj-name> : Name of the object to open the documentation for.

Options: 
--help | -h   : Print this help message.
EOF
}

if [[ $print_help == true ]] || [[ $1 == "help" ]] || [[ $1 == "--help" ]] || [[ $# -ne 1 ]]; then
  print_help
  exit 0
fi

OBJ_NAME=$1
MAN_DIR="$PROJECT_ROOT/man"

info "Generating a documentation preview for '$OBJ_NAME'..."

Rscript -e "
setwd('$PROJECT_ROOT')
mathjaxr::preview_rd(Rdfile='$OBJ_NAME.Rd')
roxygen2::roxygenize()
"

success "Done."
