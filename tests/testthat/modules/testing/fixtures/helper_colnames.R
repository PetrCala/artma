#' @title With custom colnames
#' @description For the duration of the test, register the provided column
#'   mapping in the unified per-column store (`artma.data.columns`) so that any
#'   call reading the store (e.g. `get_colnames_map()`) returns the provided
#'   mapping.
#' @param colnames_map *[list]* A named list mapping standard column names to
#'   source column names.
#' @return `NULL`, invisibly. Restores the original store after the test.
with_custom_colnames <- function(colnames_map) {
  if (!is.list(colnames_map)) {
    cli::cli_abort("The provided colnames map must be a list.")
  }
  if (length(colnames_map) == 0) {
    cli::cli_abort("The provided colnames map must not be empty.")
  }

  store <- getOption("artma.data.columns", list())
  if (!is.list(store)) store <- list()
  old_store <- store

  for (std in names(colnames_map)) {
    src <- colnames_map[[std]]
    if (is.null(src) || (length(src) == 1 && is.na(src))) next
    entry <- store[[std]]
    if (!is.list(entry)) entry <- list()
    entry$source_name <- src
    store[[std]] <- entry
  }

  options("artma.data.columns" = store)
  withr::defer(options("artma.data.columns" = old_store), envir = parent.frame())
  invisible(NULL)
}

box::export(with_custom_colnames)
