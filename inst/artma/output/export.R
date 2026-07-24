#' @title Output Export
#' @description
#' Functions for exporting analysis results (tables and graphics) to
#' a unified output directory.

is_auto_output_dir <- function(output_dir) {
  is.null(output_dir) || is.na(output_dir) || identical(output_dir, "auto")
}

#' Derive the per-options-file subdirectory for auto output
#'
#' @description
#' Uses the stem of the loaded options file (`artma.temp.file_name`) so that
#' runs driven by different options files never share an output directory.
#' The stem is sanitized to a portable character set; when no options file is
#' loaded (or the stem sanitizes to nothing), `"default"` is used.
#'
#' @return *\[character\]* A safe directory name.
#' @keywords internal
auto_output_subdir <- function() {
  file_name <- getOption("artma.temp.file_name", NULL)
  if (is.null(file_name) || length(file_name) != 1L || is.na(file_name) || !nzchar(file_name)) {
    return("default")
  }
  stem <- tools::file_path_sans_ext(basename(file_name))
  stem <- gsub("[^A-Za-z0-9._-]+", "_", stem)
  stem <- gsub("^[_.]+|[_.]+$", "", stem)
  if (!nzchar(stem)) "default" else stem
}

#' Resolve the output directory path
#'
#' @description
#' Reads the `artma.output.dir` option. If set to `"auto"` (default) or `NA`,
#' returns a durable per-options-file directory under
#' `tools::R_user_dir("artma", "data")`. Otherwise returns the configured path
#' as-is. The `"auto"` value is never rewritten in the options file; the
#' directory is resolved fresh on every call, so switching options files
#' mid-session switches the output directory with it.
#'
#' @return *\[character\]* The resolved output directory path.
resolve_output_dir <- function() {
  output_dir <- getOption("artma.output.dir", "auto")

  if (is_auto_output_dir(output_dir)) {
    return(file.path(tools::R_user_dir("artma", "data"), "results", auto_output_subdir()))
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
}

SUPPORTED_TABLE_FORMATS <- c("csv", "tex")

#' Resolve the table export formats
#'
#' @description
#' Reads the `artma.output.table_formats` option and normalises it: values are
#' lowercased and de-duplicated, unsupported ones are dropped with a warning,
#' and an empty result falls back to CSV so a run never silently produces no
#' tables.
#'
#' @return *\[character\]* The requested formats, a subset of `csv` and `tex`.
#' @keywords internal
resolve_table_formats <- function() {
  box::use(artma / libs / core / utils[get_verbosity])

  requested <- getOption("artma.output.table_formats", "csv")
  formats <- unique(tolower(as.character(requested)))
  formats <- formats[!is.na(formats)]

  unsupported <- setdiff(formats, SUPPORTED_TABLE_FORMATS)
  if (length(unsupported) > 0 && get_verbosity() >= 2) {
    cli::cli_alert_warning(
      "Ignoring unsupported table format{?s}: {.val {unsupported}}."
    )
  }

  formats <- intersect(SUPPORTED_TABLE_FORMATS, formats)
  if (length(formats) == 0) "csv" else formats
}

#' Turn a table basename into a human-readable caption
#'
#' @param name *\[character\]* The table basename.
#' @return *\[character\]* The caption text.
#' @keywords internal
table_caption <- function(name) {
  words <- strsplit(gsub("_", " ", name), " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  if (length(words) == 0) {
    return(name)
  }
  substr(words, 1, 1) <- toupper(substr(words, 1, 1))
  paste(words, collapse = " ")
}

#' Save a data frame in the configured table formats
#'
#' @param df *\[data.frame\]* The data frame to save.
#' @param name *\[character\]* File name (without extension).
#' @param output_dir *\[character\]* The base output directory.
#' @param formats *\[character\]* The formats to write, from `resolve_table_formats()`.
save_table <- function(df, name, output_dir, formats = resolve_table_formats()) {
  tables_dir <- file.path(output_dir, "tables")

  if ("csv" %in% formats) {
    utils::write.csv(df, file = file.path(tables_dir, paste0(name, ".csv")), row.names = FALSE)
  }

  if ("tex" %in% formats) {
    box::use(artma / output / latex[write_latex_table])
    write_latex_table(
      df,
      path = file.path(tables_dir, paste0(name, ".tex")),
      caption = table_caption(name),
      label = paste0("tab:", name)
    )
  }

  invisible()
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
#' result's `tables` slot is written in each configured table format (see
#' `resolve_table_formats()`); everything else (`plots`, `meta`) is ignored
#' here. There are no per-method branches.
#'
#' @param result The method's return value (a `list` with a `tables` slot).
#' @param method_name *\[character\]* The method name.
#' @param output_dir *\[character\]* The base output directory.
#' @keywords internal
export_method_result <- function(result, method_name, output_dir) {
  if (!is.list(result)) {
    return(invisible())
  }

  tables <- result$tables
  if (!is.list(tables) || length(tables) == 0L) {
    return(invisible())
  }

  formats <- resolve_table_formats()
  table_names <- names(tables)
  for (i in seq_along(tables)) {
    tbl <- tables[[i]]
    if (!is.data.frame(tbl)) next
    key <- if (is.null(table_names)) NULL else table_names[[i]]
    save_table(tbl, resolve_table_basename(method_name, key), output_dir, formats = formats)
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
