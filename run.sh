#!/usr/bin/env bash

# Function to display help
help() {
  cat <<EOF
Usage: $0 <command> [args]
  
Commands:
  R                 Invoke the R run script
  build             Build the package
  build-ignore      Add files to the package build ignore list
  bump-version      Create a new package version
  check             Check the package with devtools check (wrapped R CMD check)
  check-no-install  Check the package without installing it
  coverage          Run the coverage report
  clear-cache       Clear the cache
  deps              Install all dependencies
  desc-normalize    Normalize the DESCRIPTION file
  fix-options        Ensure that all option templates and default distributed files are valid.
  help              Display this help message
  lint              Lint all files in the R folder
  merge             Merge the currently checked out git branch with another one, and push the changes to the remote repository
  merge-pr          Merge a pull request that targets the master branch
  namespace         Generate the NAMESPACE file
  package           Invoke an R function from this package
  preview-docs      Open a preview of an exported object's documentation.
  setup             Setup the environment
  test              Run all tests
  test-e2e          Run all end-to-end tests
  vignettes         Build the vignettes
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
build) Rscript -e "devtools::build()" ;;
build-ignore) Rscript -e "usethis::use_build_ignore('$@')" ;;
bump-version) sh scripts/bumpVersion.sh "$@" ;;
check) Rscript -e "devtools::check()" ;;
check-no-install) Rscript -e "devtools::check(args = c('--no-install'))" ;;
clear-cache) sh scripts/clearCache.sh "$@" ;;
coverage) Rscript -e "covr::package_coverage()" ;;
deps) Rscript -e "devtools::install_deps(dependencies = TRUE)" ;;
desc-normalize) Rscript -e "desc::desc_normalize()" ;;
fix-options) Rscript -e "source(\"scripts/src/fix_options.R\")" ;;
lint) sh scripts/lintAll.sh "$@" ;;
merge) sh scripts/mergeAndPush.sh "$@" ;;
merge-pr) sh scripts/mergePR.sh "$@" ;;
namespace) Rscript -e "roxygen2::roxygenize()" ;;
package) sh scripts/package.sh "$@" ;;
preview-docs) sh scripts/previewDocs.sh "$@" ;;
setup) sh scripts/setup.sh "$@" ;;
test) sh scripts/test.sh "$@" ;;
test-e2e) sh scripts/testE2E.sh "$@" ;;
vignettes) Rscript -e "devtools::build_vignettes()" ;;
help) help ;;
*)
  echo "Error: Unknown command: $1"
  help
  exit 1
  ;;
esac
