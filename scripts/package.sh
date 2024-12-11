#!/usr/bin/env bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

help() {
  cat <<EOF
Usage: $0 <command> [args]
  
Commands:
  artma          Invoke the ARTMA function
EOF
}

# Check if no arguments were provided
if [ $# -ne 1 ] || [ $1 == "help" ]; then
  help
  exit 1
fi

Rscript -e "devtools::load_all(); $1(${@:2})"
