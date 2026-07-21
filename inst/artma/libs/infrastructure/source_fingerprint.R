# Source-code fingerprints for cache keys.
#
# The on-disk cache keys on the user's inputs (data, options, upstream
# results). None of that changes when the package's own code changes, so a
# cached artifact would survive an edit to the very function that produced it.
# These helpers close that gap by folding a hash of the package source into the
# cache signature.
#
# Fingerprints are computed once per session and memoised in `.state`. Session
# scope is the right granularity: `box` caches loaded modules for the lifetime
# of the session, so an edit made mid-session does not take effect until the
# modules are reloaded, which resets this env too.

box::use(
  artma / paths[PATHS]
)

.state <- new.env(parent = emptyenv())

#' @title Hash a set of source files
#' @description Hash file contents together with their paths relative to
#'   `root`, so both edits and renames change the result. Files are sorted by
#'   path to keep the hash independent of the filesystem's listing order.
#' @param files *\[character\]* Absolute paths of the files to hash.
#' @param root *\[character\]* Directory the paths are made relative to.
#' @return *\[character\]* A single hash, or `NA_character_` when `files` is
#'   empty.
#' @keywords internal
hash_source_files <- function(files, root) {
  if (length(files) == 0L) {
    return(NA_character_)
  }

  files <- sort(files, method = "radix")

  entries <- lapply(files, function(path) {
    contents <- tryCatch(
      paste(readLines(path, warn = FALSE), collapse = "\n"),
      error = function(err) NA_character_
    )
    list(path = substring(path, nchar(root) + 2L), contents = contents)
  })

  rlang::hash(entries)
}

#' @title Fingerprint the package source tree
#' @description Hash every R source file under the package's module root. Any
#'   change to the package's own code (a method, a shared calculation helper, an
#'   output formatter) changes the fingerprint and therefore every cache key
#'   built from it.
#' @param root *\[character, optional\]* Module root to fingerprint. Defaults to
#'   `PATHS$PROJECT_ROOT`.
#' @return *\[character\]* A single hash, or `NA_character_` when the root does
#'   not exist.
package_source_fingerprint <- function(root = PATHS$PROJECT_ROOT) {
  cache_key <- paste0("tree:", root)
  cached <- .state[[cache_key]]
  if (!is.null(cached)) {
    return(cached)
  }

  if (!dir.exists(root)) {
    fingerprint <- NA_character_
  } else {
    files <- list.files(root, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
    fingerprint <- hash_source_files(files, root)
  }

  .state[[cache_key]] <- fingerprint
  fingerprint
}

#' @title Hash a single runtime method's source file
#' @description Hash the source file backing a runtime method, identified by its
#'   stage (method files are named `<stage>.R`). Editing the method changes the
#'   hash and invalidates only that method's cache, whereas
#'   `package_source_fingerprint()` invalidates everything. Both feed the
#'   signature: the per-method hash keeps the common case precise, the tree
#'   fingerprint covers the shared code a method calls into.
#' @param stage *\[character\]* The method's stage label.
#' @param methods_dir *\[character, optional\]* Directory holding the method
#'   files. Defaults to `PATHS$DIR_METHODS`.
#' @return *\[character\]* A single hash, or `NA_character_` when no matching
#'   file exists.
method_source_hash <- function(stage, methods_dir = PATHS$DIR_METHODS) {
  if (!is.character(stage) || length(stage) != 1L || is.na(stage)) {
    return(NA_character_)
  }

  cache_key <- paste0("method:", methods_dir, ":", stage)
  cached <- .state[[cache_key]]
  if (!is.null(cached)) {
    return(cached)
  }

  path <- file.path(methods_dir, paste0(stage, ".R"))
  hash <- if (file.exists(path)) hash_source_files(path, methods_dir) else NA_character_

  .state[[cache_key]] <- hash
  hash
}

#' @title Forget memoised source fingerprints
#' @description Drop the session-scoped fingerprint cache. Used by tests that
#'   edit source files and need the next fingerprint to reflect the change.
#' @return `NULL`, invisibly.
#' @keywords internal
forget_source_fingerprints <- function() {
  rm(list = ls(envir = .state, all.names = TRUE), envir = .state)
  invisible(NULL)
}

box::export(
  forget_source_fingerprints,
  hash_source_files,
  method_source_hash,
  package_source_fingerprint
)
