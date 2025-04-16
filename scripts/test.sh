#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

help() {
  cat <<EOF
  Usage: $0 <command> [args]

  Commands:
    --file <file>  Run a specific test file within the '$TEST_DIR_NAME' directory
    --filter <filter>  Run tests with a specific filter
    --help  Display this help message
EOF
}

while [[ "$#" -gt 0 ]]; do
  case $1 in
  --file)
    FILE="$2"
    shift
    ;;
  --filter)
    FILTER="$2"
    shift
    ;;
  --help | -h)
    help
    exit 1
    ;;
  *)
    echo "Unknown parameter passed: $1"
    exit 1
    ;;
  esac
  shift
done

# Show help if no arguments or help flags are provided
if [ $# -eq 0 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
  help
  exit 1
fi

if [[ -n "$FILE" ]]; then
  FULL_PATH="$TEST_DIR_NAME/$FILE"
  if [[ ! -f "$FULL_PATH" ]]; then
    echo "Error: File '$FILE' does not exist in the '$TEST_DIR_NAME' directory"
    exit 1
  fi
  Rscript -e "devtools::test_active_file('$FULL_PATH')"
elif [[ -n "$FILTER" ]]; then
  Rscript -e "devtools::test(filter = '$FILTER')"
else
  Rscript -e "devtools::test()"
fi
