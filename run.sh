#!/usr/bin/env bash

# Function to display help
help() {
  cat <<EOF
Usage: $0 <command> [args]
  
Commands:
  R               Invoke the R run script
  bump-version    Create a new package version
  clear-cache     Clear the cache
  config           Configure the project
  deps            Install all dependencies
  fix-options      Ensure that all option templates and default distributed files are valid.
  help            Display this help message
  lint            Lint all files in the R folder
  merge           Merge the currently checked out git branch with another one, and push the changes to the remote repository
  namespace       Generate the NAMESPACE file
  package         Invoke an R function from this package
  setup           Setup the environment
  test            Run all tests
  test-e2e        Run all end-to-end tests
EOF
}

# Check if no arguments were provided
if [ $# -eq 0 ]; then
  help
  exit 1
fi

cd "$(dirname "${BASH_SOURCE[0]}")"

RUN_ARG=$1
shift

# Main switch-case to handle commands
case "$RUN_ARG" in
[Rr]) sh scripts/runR.sh "$@" ;;
bump-version) sh scripts/bumpVersion.sh "$@" ;;
clear-cache) sh scripts/clearCache.sh "$@" ;;
config) sh scripts/config.sh "$@" ;;
deps) Rscript -e "devtools::install_deps(dependencies = TRUE)" ;;
fix-options) Rscript -e "source(\"scripts/src/fix_options.R\")" ;;
lint) sh scripts/lintAll.sh "$@" ;;
merge) sh scripts/mergeAndPush.sh "$@" ;;
namespace) Rscript -e "roxygen2::roxygenize()" ;;
package) sh scripts/package.sh "$@" ;;
setup) sh scripts/setup.sh "$@" ;;
test) sh scripts/test.sh "$@" ;;
test-e2e) sh scripts/testE2E.sh "$@" ;;
help) help ;;
*)
  echo "Error: Unknown command: $1"
  help
  exit 1
  ;;
esac
