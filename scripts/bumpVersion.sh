#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

Rscript -e "
  source(\"$SCRIPTS_SRC_DIR/bump_version.R\")
  bump_version(element=\"patch\")
"
