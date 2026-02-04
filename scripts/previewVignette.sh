#!/bin/bash

set -e

. "$(dirname "${BASH_SOURCE[0]}")/common.sh"
shopt -s nullglob

function print_help {
  cat <<EOF
Usage: ./previewVignette.sh [<options>] <vignette>

Description: Render a vignette and open the HTML preview in your browser.

Args:
<vignette> : Vignette name or path. Accepts 'getting-started',
             'getting-started.Rmd', or 'vignettes/getting-started.Rmd'.

Options:
--list      : Print available vignettes.
--help | -h : Print this help message.
EOF
}

if [[ $1 == "--help" ]] || [[ $1 == "-h" ]] || [[ $1 == "help" ]]; then
  print_help
  exit 0
fi

if [[ $1 == "--list" ]]; then
  info "Available vignettes:"
  for vignette in "$PROJECT_ROOT/vignettes"/*.Rmd; do
    [ -e "$vignette" ] || continue
    echo "  $(basename "$vignette" .Rmd)"
  done
  exit 0
fi

if [[ $# -gt 1 ]]; then
  print_help
  exit 1
fi

VIGNETTE_INPUT=""

if [[ $# -eq 0 ]]; then
  vignette_files=("$PROJECT_ROOT/vignettes"/*.Rmd)

  if [[ ${#vignette_files[@]} -eq 0 ]]; then
    error "No vignettes found in $PROJECT_ROOT/vignettes."
    exit 1
  fi

  vignette_names=()
  for vignette in "${vignette_files[@]}"; do
    vignette_names+=("$(basename "$vignette" .Rmd)")
  done

  options=("${vignette_names[@]}" "Cancel")
  PS3="Select a vignette (or Cancel): "
  select choice in "${options[@]}"; do
    if [[ -z "$choice" ]]; then
      info "Invalid selection. Please choose a number from the list."
      continue
    fi
    if [[ "$choice" == "Cancel" ]]; then
      info "Cancelled."
      exit 0
    fi
    VIGNETTE_INPUT="$choice"
    break
  done
else
  VIGNETTE_INPUT=$1
fi
VIGNETTE_PATH=""

if [[ -f "$VIGNETTE_INPUT" ]]; then
  VIGNETTE_PATH=$(get_abs_path "$VIGNETTE_INPUT")
else
  VIGNETTE_NAME=$(basename "$VIGNETTE_INPUT")
  VIGNETTE_NAME=${VIGNETTE_NAME%.Rmd}
  VIGNETTE_PATH="$PROJECT_ROOT/vignettes/$VIGNETTE_NAME.Rmd"
fi

if [[ ! -f "$VIGNETTE_PATH" ]]; then
  error "Vignette not found: $VIGNETTE_PATH"
  info "Run './previewVignette.sh --list' to see available vignettes."
  exit 1
fi

OUTPUT_DIR="$PROJECT_ROOT/doc"
mkdir -p "$OUTPUT_DIR"

info "Rendering vignette: $VIGNETTE_PATH"

Rscript -e "
setwd('$PROJECT_ROOT')
if (requireNamespace('devtools', quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}
output_file <- rmarkdown::render('$VIGNETTE_PATH', output_dir = '$OUTPUT_DIR')
utils::browseURL(output_file)
"

success "Done."
