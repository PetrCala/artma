#!/bin/bash

# Check if a directory argument is provided
if [ $# -gt 1 ]; then
    echo "Usage: $0 [<directory>]"
    exit 1
fi

COMMON_FILE_PATH=$(dirname "${BASH_SOURCE[0]}")/common.sh

# Ensure that the shell script can find and use 'source' by using a portable syntax
if [ -f "$COMMON_FILE_PATH" ]; then
    . "$COMMON_FILE_PATH"
else
    echo "Error: shellUtils.sh not found"
    exit 1
fi

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

# Lint all R files in the directory
LINT_OUTPUT=$(Rscript -e "lintr::lint_dir('$LINT_DIR')" 2>&1 | tee /dev/tty)

# An option with custom linters
# Rscript -e "source('$CUSTOM_LINTERS_FILE_PATH'); lintr::lint_dir('$LINT_DIR')"

if [[ -n "$LINT_OUTPUT" ]]; then
    error "Linting failed"
    exit 1
fi

# Run custom linting functions
# Rscript -e "source(\"$CUSTOM_LINTERS_FILE_PATH\")" "$LINT_DIR"

success "Linting successful!"
