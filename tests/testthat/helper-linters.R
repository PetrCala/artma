# Load the repository's custom linters from scripts/linters.R.
#
# The scripts directory is development tooling and is excluded from the built
# package (.Rbuildignore), so the file is only reachable when the tests run
# next to a full repository checkout. Returns NULL when the file cannot be
# located; callers should skip in that case. Requires the lintr package, so
# callers must also run testthat::skip_if_not_installed("lintr") first.
load_repo_linters <- function() {
  path <- getwd()
  while (!file.exists(file.path(path, "DESCRIPTION")) && dirname(path) != path) {
    path <- dirname(path)
  }

  linters_path <- file.path(path, "scripts", "linters.R")
  if (!file.exists(linters_path)) {
    return(NULL)
  }

  env <- new.env()
  source(linters_path, local = env) # nolint: undesirable_function_linter.
  env
}
