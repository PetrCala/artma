#' @title Output Export
#' @description
#' Functions for exporting analysis results (tables and graphics) to
#' a unified output directory.

is_auto_output_dir <- function(output_dir) {
  is.null(output_dir) || is.na(output_dir) || identical(output_dir, "auto")
}

persist_auto_output_dir <- function(output_dir) {
  box::use(artma / libs / core / utils[get_verbosity])

  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")

  if (is.null(options_file_name) || is.null(options_dir)) {
    if (get_verbosity() >= 4) {
      cli::cli_inform("No options file available to persist output directory.")
    }
    return(invisible(FALSE))
  }

  saved <- tryCatch(
    {
      suppressMessages(
        artma::options.modify(
          options_file_name = options_file_name,
          options_dir = options_dir,
          user_input = list("output.dir" = output_dir),
          should_validate = TRUE
        )
      )
      TRUE
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Failed to persist output directory: {e$message}"
        )
      }
      FALSE
    }
  )

  if (isTRUE(saved)) {
    options("artma.output.dir" = output_dir)
  }

  invisible(saved)
}

#' Resolve the output directory path
#'
#' @description
#' Reads the `artma.output.dir` option. If set to `"auto"` (default) or `NA`,
#' returns a session-specific temporary directory. Otherwise returns the
#' configured path as-is.
#'
#' @return *\[character\]* The resolved output directory path.
resolve_output_dir <- function() {
  output_dir <- getOption("artma.output.dir", "auto")

  if (is_auto_output_dir(output_dir)) {
    return(file.path(tempdir(), "artma-results"))
  }

  output_dir
}

#' Resolve the graphics subdirectory path
#'
#' @description
#' Reads the `artma.visualization.export_path` option (default: `"graphics"`)
#' and resolves it relative to the given output directory.
#'
#' @param output_dir *\[character\]* The base output directory.
#' @return *\[character\]* The resolved graphics directory path.
resolve_graphics_dir <- function(output_dir) {
  export_path <- getOption("artma.visualization.export_path", "graphics")
  file.path(output_dir, export_path)
}

#' Ensure output directories exist
#'
#' @description
#' Creates the output directory and its subdirectories (`tables`, graphics)
#' if they do not already exist.
#'
#' @param output_dir *\[character\]* The base output directory.
ensure_output_dirs <- function(output_dir) {
  dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(resolve_graphics_dir(output_dir), recursive = TRUE, showWarnings = FALSE)

  if (is_auto_output_dir(getOption("artma.output.dir", "auto"))) {
    persist_auto_output_dir(output_dir)
  }
}

#' Save a data frame as CSV
#'
#' @param df *\[data.frame\]* The data frame to save.
#' @param name *\[character\]* File name (without extension).
#' @param output_dir *\[character\]* The base output directory.
save_table <- function(df, name, output_dir) {
  path <- file.path(output_dir, "tables", paste0(name, ".csv"))
  utils::write.csv(df, file = path, row.names = FALSE)
}

#' Export all results to the output directory
#'
#' @description
#' Iterates over a named list of method results and exports each method's
#' tabular data (the `tables` slot of the standard return contract) as CSV
#' files. Graphics are written by each method during execution, so plot-only
#' methods simply contribute no tables here.
#'
#' @param results *\[list\]* Named list of method results from `invoke_runtime_methods()`.
#' @param output_dir *\[character\]* The base output directory.
export_results <- function(results, output_dir) {
  for (method_name in names(results)) {
    result <- results[[method_name]]
    if (is.null(result)) next

    tryCatch(
      export_method_result(result, method_name, output_dir),
      error = function(e) {
        box::use(artma / libs / core / utils[get_verbosity])
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning("Failed to export {.val {method_name}}: {e$message}")
        }
      }
    )
  }

  write_last_export_marker(output_dir)
}

#' Resolve the CSV basename for a table in the standard return contract
#'
#' @description
#' A table keyed with a generic label (`summary`, `coefficients`, `table`), an
#' empty key, or the method name itself is written as `<method_name>.csv`. Any
#' other key is treated as a sub-table and written as `<method_name>_<key>.csv`
#' (for example the caliper/elliott/maive tables of `p_hacking_tests`).
#'
#' @param method_name *\[character\]* The method name.
#' @param key *\[character\]* The table's name within the `tables` list.
#' @return *\[character\]* The CSV basename (without extension).
#' @keywords internal
resolve_table_basename <- function(method_name, key) {
  generic_keys <- c("summary", "coefficients", "table")
  if (is.null(key) || is.na(key) || !nzchar(key) ||
    identical(key, method_name) || key %in% generic_keys) {
    return(method_name)
  }
  paste0(method_name, "_", key)
}

#' Export a single method's result
#'
#' @description
#' A generic walk over the standard return contract. Every `data.frame` in the
#' result's `tables` slot is written as a CSV; everything else (`plots`, `meta`)
#' is ignored here. There are no per-method branches.
#'
#' @param result The method's return value (a `list` with a `tables` slot).
#' @param method_name *\[character\]* The method name.
#' @param output_dir *\[character\]* The base output directory.
#' @keywords internal
export_method_result <- function(result, method_name, output_dir) {
  if (!is.list(result)) return(invisible())

  tables <- result$tables
  if (!is.list(tables) || length(tables) == 0L) return(invisible())

  table_names <- names(tables)
  for (i in seq_along(tables)) {
    tbl <- tables[[i]]
    if (!is.data.frame(tbl)) next
    key <- if (is.null(table_names)) NULL else table_names[[i]]
    save_table(tbl, resolve_table_basename(method_name, key), output_dir)
  }

  invisible()
}

write_last_export_marker <- function(output_dir) {
  box::use(artma / paths[PATHS])

  marker_dir <- PATHS$DIR_USR_CACHE
  dir.create(marker_dir, recursive = TRUE, showWarnings = FALSE)

  marker_path <- file.path(marker_dir, "last_export_dir")
  tryCatch(
    writeLines(normalizePath(output_dir, mustWork = FALSE), marker_path),
    error = function(e) NULL
  )

  invisible(marker_path)
}

box::export(
  resolve_output_dir,
  resolve_graphics_dir,
  ensure_output_dirs,
  export_results
)
