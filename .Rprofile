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
