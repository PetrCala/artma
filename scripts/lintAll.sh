#!/bin/bash

set -e

# Check if a directory argument is provided
if [ $# -gt 1 ]; then
    echo "Usage: $0 [<directory>]"
    exit 1
fi

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

# Use the provided directory if available in the argument
if [ $# -eq 1 ]; then
    LINT_DIR=$(get_abs_path "$1")
fi

# Define the path to custom linters
CUSTOM_LINTERS_FILE_PATH="$LINT_DIR/custom_linters.R"

# Check if R and the required package are available
if ! command -v Rscript &>/dev/null; then
    echo "Error: Rscript is not installed"
    exit 1
fi

if ! Rscript -e "if (!requireNamespace('lintr', quietly = TRUE)) quit(status = 1)" &>/dev/null; then
    echo "Error: R package 'lintr' is not installed"
    exit 1
fi

Rscript -e "
devtools::load_all()
artma::static_setup() # Ensures the correct boxpath and static paths
"

LINT_OUTPUT=$(Rscript -e "lintr::lint_package()" 2>&1 | tee /dev/tty)

if [[ "$LINT_OUTPUT" != *"No lints found"* ]]; then
    error "Linting failed"
    exit 1
fi
