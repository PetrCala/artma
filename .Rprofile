# Source global .Rprofile if it exists
if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile", local = TRUE) # nolint: undesirable_function_linter.
}

# Pre-load lintr settings so that the R language server respects .lintr.R.
# This is needed because languageserver calls lintr::lint(path, text=content),
# which sets parse_settings=FALSE, skipping config discovery entirely.
# See: https://github.com/REditorSupport/languageserver
# read_settings() is unexported in lintr; guard it in case it is removed or renamed.
if (requireNamespace("lintr", quietly = TRUE)) {
  tryCatch(
    lintr:::read_settings(getwd()), # nolint: undesirable_function_linter.
    error = function(e) invisible(NULL)
  )
}

# Dev convenience: `artma_hub()` loads the package and opens the interactive
# session hub in one call, instead of `devtools::load_all(); artma::artma()`.
# Only defined interactively: `interactive()` must be TRUE for the hub to open
# anyway, and this must run from a real `R` session (not `R -e`/`Rscript`,
# which are non-interactive regardless of a live terminal) for that to hold.
if (interactive() && requireNamespace("devtools", quietly = TRUE)) {
  artma_hub <- function() { # nolint: object_name_linter.
    devtools::load_all(quiet = TRUE)
    artma::artma()
  }

  # Set ARTMA_DEV_HUB=1 (e.g. via the `artma-hub` shell function) to open the
  # hub automatically on startup instead of calling `artma_hub()` yourself.
  if (nzchar(Sys.getenv("ARTMA_DEV_HUB"))) {
    artma_hub()
  }
}
