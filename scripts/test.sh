#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

export TESTTHAT=true

TEST_FOLDER_PATH="$PROJECT_ROOT/$TEST_DIR_NAME"

Rscript -e "
testthat::test_dir("tests")
"

unset TESTTHAT

exit 0
