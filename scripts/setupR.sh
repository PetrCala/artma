#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

info "Setting up the R environment..."

Rscript -e "
  source(\"$SCRIPTS_SRC_DIR/env.R\")
  source(\"$SCRIPTS_SRC_DIR/packages.R\")
  load_packages(PACKAGES)
"

info "Finished setting up the R environment."
