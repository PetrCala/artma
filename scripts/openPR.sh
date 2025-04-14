#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

PERSONAL_USER="PetrCala"
SHOULD_RELEASE=false

# Check for uncommitted changes
if [[ -n $(git status --porcelain) ]]; then
  error "There are uncommitted changes in the repository. Please commit or stash them before proceeding."
  exit 1
fi

# Install gh if it's not already installed
if ! command -v gh &>/dev/null; then
  error "gh is not installed. Please install it first."
  exit 1
fi

# Check if the PR should release a new version
while [[ "$#" -gt 0 ]]; do
  case $1 in
  --release)
    SHOULD_RELEASE=true
    shift
    ;;
  *) shift ;;
  esac
done

CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
info "Current branch: $CURRENT_BRANCH"
info "Should release: $SHOULD_RELEASE"

LABEL_ARG=""
if [[ $SHOULD_RELEASE == "true" ]]; then
  LABEL_ARG="--label release:next-version"
fi

# Merge the PR using your personal account
gh auth switch --hostname github.com --user $PERSONAL_USER
gh pr create \
  --base master \
  --head $CURRENT_BRANCH \
  --title "$CURRENT_BRANCH" \
  --body - \
  --fill-verbose \
  $LABEL_ARG \
  --assignee "@me"

success "Done!"
