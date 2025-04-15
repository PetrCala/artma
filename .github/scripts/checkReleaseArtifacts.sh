#!/bin/bash

set -e

RELEASE_ARTIFACTS_FILE="$(dirname "$0")/../workflows/release-artifacts.txt"

if [ ! -f "$RELEASE_ARTIFACTS_FILE" ]; then
  echo "❌ Missing expected file: $RELEASE_ARTIFACTS_FILE"
  exit 1
fi

RELEASE_ARTIFACTS=$(paste -sd " " "$RELEASE_ARTIFACTS_FILE")

missing=0

for file in $RELEASE_ARTIFACTS; do
  if ! compgen -G "$file" >/dev/null; then
    echo "❌ Missing expected file: $file"
    missing=1
  fi
done

if [ "$missing" -eq 1 ]; then
  echo "One or more expected files are missing. Failing the job."
  exit 1
fi
