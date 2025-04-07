#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

Rscript -e "devtools::test()"

exit 0
