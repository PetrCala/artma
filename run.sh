#!/usr/bin/env bash

# Function to display help
help() {
  cat <<EOF
Usage: $0 <command> [args]
  
Commands:
  R               Invoke the R run script
  config           Configurate the project
  clear-cache     Clear the cache
  deps            Install all dependencies
  lint            Lint all files in the R folder
  merge           Merge the currently checked out git branch with another one, and push the changes to the remote repository
  setup           Setup the environment
  test            Run all tests
  help            Display this help message
EOF
}

# Check if no arguments were provided
if [ $# -eq 0 ]; then
  help
  exit 1
fi

cd "$(dirname "${BASH_SOURCE[0]}")"

# Main switch-case to handle commands
case "$1" in
[Rr]) sh scripts/runR.sh "${@:2}" ;;
config) sh scripts/config.sh "${@:2}" ;;
clear-cache) sh scripts/clearCache.sh "${@:2}" ;;
deps) Rscript -e "devtools::install_deps(dependencies = TRUE)" ;;
lint) sh scripts/lintAll.sh "${@:2}" ;;
merge) sh scripts/mergeAndPush.sh "${@:2}" ;;
setup) sh scripts/setup.sh "${@:2}" ;;
test) sh scripts/test.sh "${@:2}" ;;
help) help ;;
*)
  echo "Error: Unknown command: $1"
  help
  exit 1
  ;;
esac
