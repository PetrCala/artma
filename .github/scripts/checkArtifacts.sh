#!/bin/bash

# Verifies that all required artifacts exist before proceeding with GitHub Actions workflow.
# Takes a file containing a list of expected artifacts and validates their presence.
# Fails if any required files are missing.

set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <artifacts_file_name>"
  exit 1
fi

ARTIFACTS_FILE_NAME="$1"
ARTIFACTS_FILE="$(dirname "$0")/$ARTIFACTS_FILE_NAME"
if [ ! -f "$ARTIFACTS_FILE" ]; then
  echo "❌ Missing expected file: $ARTIFACTS_FILE"
  exit 1
fi

ARTIFACTS=$(paste -sd " " "$ARTIFACTS_FILE")

missing=0

for file in $ARTIFACTS; do
  if ! compgen -G "$file" >/dev/null; then
    echo "❌ Missing expected file: $file"
    missing=1
  fi
done

if [ "$missing" -eq 1 ]; then
  echo "One or more expected files are missing. Failing the job."
  exit 1
fi
