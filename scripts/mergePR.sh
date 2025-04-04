#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"

# Get the PR number from the arguments
PR_NUMBER=$1

ARTMABOT_HOST=artmabot.github.com
PERSONAL_HOST=PetrCala.github.com
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

# Approve the PR as ArtmaBot
gh pr review $PR_NUMBER --hostname $ARTMABOT_HOST -a
info "Approved PR as ArtmaBot."

# Merge the PR using your personal account
gh pr merge $PR_NUMBER --hostname $PERSONAL_HOST --rebase --auto
info "Merged PR using your personal account."

# Delete origin and local branches
git push origin --delete $CURRENT_BRANCH
git checkout master
git branch -D $CURRENT_BRANCH
git pull

success "Done!"
