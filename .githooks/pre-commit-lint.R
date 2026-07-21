#!/usr/bin/env Rscript

# Styles and lints staged .R files for the pre-commit hook.
#
# Usage: Rscript pre-commit-lint.R <repo_root> <file1> [file2 ...]
#
# Exit codes:
#   0 - clean (files may have been auto-styled; see ::STYLED:: markers)
#   1 - lintr found issues; the commit should be blocked
#   2 - internal error (missing packages, unexpected failure); the caller
#       should warn and let the commit through rather than block on this

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  quit(status = 0, save = "no")
}

repo_root <- args[[1]]
files <- args[-1]

result <- tryCatch(
  {
    if (!requireNamespace("styler", quietly = TRUE) || !requireNamespace("lintr", quietly = TRUE)) {
      stop("styler and/or lintr package not installed", call. = FALSE)
    }

    options(box.path = file.path(repo_root, "inst"))

    styled_files <- character(0)
    for (f in files) {
      before <- readLines(f, warn = FALSE)
      invisible(utils::capture.output(styler::style_file(f)))
      after <- readLines(f, warn = FALSE)
      if (!identical(before, after)) {
        styled_files <- c(styled_files, f)
      }
    }

    if (length(styled_files) > 0) {
      cat("::STYLED::\n")
      cat(paste(styled_files, collapse = "\n"), "\n", sep = "")
      cat("::END_STYLED::\n")
    }

    lint_failures <- FALSE
    for (f in files) {
      lints <- lintr::lint(f)
      if (length(lints) > 0) {
        lint_failures <- TRUE
        cat("::LINT_FAIL::", f, "\n", sep = "")
        print(lints)
      }
    }

    if (lint_failures) 1L else 0L
  },
  error = function(e) {
    cat("::INTERNAL_ERROR::", conditionMessage(e), "\n", sep = "")
    2L
  }
)

quit(status = result, save = "no")
