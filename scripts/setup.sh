#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

NO_BOX_CHECK=false

while [[ "$#" -gt 0 ]]; do
    case $1 in
    --no-box-check) NO_BOX_CHECK=true ;;
    *)
        error "Unknown option: $1"
        exit 1
        ;;
    esac
    shift
done

if ! command -v R &>/dev/null; then
    error "R is not installed. Make sure to install it from https://www.cran.r-project.org."
    exit 1
fi

info "Setting up the environment..."

cd $PROJECT_ROOT

# devtools no longer ships remotes as a hard dependency, so install the
# dependencies with remotes explicitly before installing the package itself.
Rscript -e "if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes')"
Rscript -e "remotes::install_deps(dependencies = TRUE, upgrade = 'never')"
Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
Rscript -e "devtools::install(dependencies = FALSE)"

# Validate box.path
if [[ $NO_BOX_CHECK == false ]]; then
    if ! grep -q "box.path" ~/.Rprofile; then
        BOX_PATH_ERR_MSG="box.path is not set in ~/.Rprofile. Please add the following line to your ~/.Rprofile file:"
        BOX_PATH_ERR_MSG+="options(box.path = \"$PROJECT_ROOT/inst\")"
        error "$BOX_PATH_ERR_MSG"
        exit 1
    fi
fi

success "Done."
