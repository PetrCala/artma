#!/bin/bash
set -e

SCRIPTS_DIR=$(dirname "${BASH_SOURCE[0]}")
source "$SCRIPTS_DIR/shellUtils.sh"

if [[ $(git status --porcelain) ]]; then
    error "There are unsaved changes. Please commit or stash your changes before running this script."
    exit 1
fi

if [ $# -eq 0 ]; then
    error "No branch name provided. Please specify the branch name."
    exit 1
fi

target_branch_name=$1
current_branch=$(git rev-parse --abbrev-ref HEAD)

# Output the branch name
info "Merging from branch $current_branch to branch $target_branch_name..."

git checkout $target_branch_name
git merge $current_branch
git push origin $target_branch_name
git checkout $current_branch

success "Merge complete!"