#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

# Get the PR number from the arguments
PR_NUMBER=$1
EXPECTED_ACTOR="ArtmaBot"

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

CURRENT_ACTOR=$(gh api user --jq '.login')

if [ "$CURRENT_ACTOR" != "$EXPECTED_ACTOR" ]; then
  error "This script must be run by $EXPECTED_ACTOR. Current actor is $CURRENT_ACTOR."
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

# Get the current branch name
CURRENT_BRANCH=$(git branch --show-current)

# Review the PR
gh pr review $PR_NUMBER -a

# Merge the PR
gh pr merge $PR_NUMBER --rebase --auto

# Delete the origin branch
git push origin --delete $CURRENT_BRANCH

# Delete the local branch
git checkout master
git branch -D $CURRENT_BRANCH
git pull

success "Done!"
