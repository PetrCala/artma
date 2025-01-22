#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

E2E_TEST_FOLDER_PATH="$PROJECT_ROOT/$E2E_TEST_DIR_NAME"

cd $E2E_TEST_FOLDER_PATH

TEST_FILES=($(find . -name "test_*.R"))
TEST_FILES_COUNT=${#TEST_FILES[@]}

info "Running $TEST_FILES_COUNT end-to-end tests..."

for TEST_FILE in $TEST_FILES; do
  info "Testing $TEST_FILE..."
  if ! Rscript "$TEST_FILE" 2>&1; then
    error "Error in $TEST_FILE" >&2
    exit 1
  fi
done

success "Done!"
exit 0
