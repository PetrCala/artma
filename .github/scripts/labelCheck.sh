#!/bin/bash

set -e

label_names="$1"

ready_to_build="false"
skip_cran="false"

if echo "$label_names" | jq -e '.[] | select(.name == "release:next-version")' >/dev/null; then
  ready_to_build="true"
fi

if echo "$label_names" | jq -e '.[] | select(.name == "release:skip-cran")' >/dev/null; then
  skip_cran="true"
fi

semver_labels=$(echo "$label_names" | jq -r '[.[] | select(.name | startswith("v-")) | .name]')
label_count=$(echo "$semver_labels" | jq 'length')

semver_level="patch"
if ((label_count > 1)); then
  echo "Error: more than one label starting with 'v-' found." >&2
  exit 1
elif ((label_count == 1)); then
  semver_level=$(echo "$semver_labels" | jq -r '.[0]' | sed 's/^v-//')
fi

echo "READY_TO_BUILD=$ready_to_build" >>"$GITHUB_OUTPUT"
echo "SEMVER_LEVEL=$semver_level" >>"$GITHUB_OUTPUT"
echo "SKIP_CRAN=$skip_cran" >>"$GITHUB_OUTPUT"
