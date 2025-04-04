#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

# Get the PR number from the arguments
PR_NUMBER=$1

ARTMABOT_USER="ArtmaBot"
PERSONAL_USER="PetrCala"

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

if [ -z "$PR_NUMBER" ]; then
  info "PR number not provided. Determining the latest open PR..."
  PR_NUMBER=$(gh pr list --state open --limit 1 --json number --jq '.[0].number')
  if [ -z "$PR_NUMBER" ]; then
    error "No open PRs found."
    exit 1
  fi
  info "Using the latest open PR: #$PR_NUMBER"
fi

CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
info "Current branch: $CURRENT_BRANCH"

# Approve the PR as ArtmaBot
gh auth switch --hostname github.com --user $ARTMABOT_USER
gh pr review $PR_NUMBER -a
info "Approved PR as ArtmaBot."

# Merge the PR using your personal account
gh auth switch --hostname github.com --user $PERSONAL_USER
gh pr merge $PR_NUMBER --rebase --auto
info "Merged PR using your personal account."

# Delete origin and local branches
git push origin --delete origin $CURRENT_BRANCH
git checkout master
git branch -D $CURRENT_BRANCH
git pull

success "Done!"
