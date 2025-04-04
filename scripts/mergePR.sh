#!/bin/bash

set -e

# Get the PR number from the arguments
PR_NUMBER=$1

if [ -z "$PR_NUMBER" ]; then
  echo "Usage: mergePR.sh <PR_NUMBER>"
  exit 1
fi

# Install gh if it's not already installed
if ! command -v gh &>/dev/null; then
  echo "gh is not installed. Please install it first."
  exit 1
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

echo "Done!"
