#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

if ! command -v R &>/dev/null; then
    error "R is not installed. Make sure to install it from https://www.cran.r-project.org."
    exit 1
fi

info "Setting up the environment..."

cd $PROJECT_ROOT

if [[ ! -d $CACHE_DIR ]]; then
    info "Creating cache directory..."
    mkdir $CACHE_DIR
fi

source "$SCRIPTS_DIR/setupR.sh" # Prints verbose output

# Validate box.path
if ! grep -q "box.path" ~/.Rprofile; then
    BOX_PATH_ERR_MSG="box.path is not set in ~/.Rprofile. Please add the following line to your ~/.Rprofile file:"
    BOX_PATH_ERR_MSG+="options(box.path = \"$PROJECT_ROOT/inst\")"
    error "$BOX_PATH_ERR_MSG"
    exit 1
fi

success "Done."
