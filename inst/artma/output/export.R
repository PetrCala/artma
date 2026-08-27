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
#' @param file_name *\[character, optional\]* Options file name to derive the
#'   subdirectory from. Defaults to the loaded options file, so callers that
#'   inspect a file without loading it can pass the name explicitly.
#' @return *\[character\]* A safe directory name.
#' @keywords internal
auto_output_subdir <- function(file_name = getOption("artma.temp.file_name", NULL)) {
  if (is.null(file_name) || length(file_name) != 1L || is.na(file_name) || !nzchar(file_name)) {
    return("default")
  }
  stem <- tools::file_path_sans_ext(basename(file_name))
  stem <- gsub("[^A-Za-z0-9._-]+", "_", stem)
  stem <- gsub("^[_.]+|[_.]+$", "", stem)
  if (!nzchar(stem)) "default" else stem
}

#' Resolve the base output directory path
#'
#' @description
#' Reads the `artma.output.dir` option. If set to `"auto"` (default) or `NA`,
#' returns a durable per-options-file directory under
#' `tools::R_user_dir("artma", "data")`. Otherwise returns the configured path
#' as-is. The `"auto"` value is never rewritten in the options file; the
#' directory is resolved fresh on every call, so switching options files
#' mid-session switches the output directory with it.
#'
#' This is the directory the options file points at. With
#' `output.run_subdirectories` enabled, a run writes into a timestamped
#' subdirectory of it instead; see [resolve_output_dir()].
#'
#' @return *\[character\]* The resolved base output directory path.
resolve_base_output_dir <- function() {
  output_dir <- getOption("artma.output.dir", "auto")

  if (is_auto_output_dir(output_dir)) {
    return(file.path(tools::R_user_dir("artma", "data"), "results", auto_output_subdir()))
  }

  output_dir
}

# The directory holding the per-run subdirectories, and the runtime option
# recording the directory of the run currently executing.
RUNS_DIR_NAME <- "runs"
ACTIVE_RUN_DIR_OPTION <- "artma.temp.run_output_dir"

#' Check whether runs write into timestamped subdirectories
#'
#' @return *\[logical\]* The `artma.output.run_subdirectories` option.
#' @keywords internal
use_run_subdirectories <- function() {
  isTRUE(getOption("artma.output.run_subdirectories", FALSE))
}

#' Read the directory of the run currently executing
#'
#' @return *\[character or NULL\]* The active run directory, if any.
#' @keywords internal
active_run_output_dir <- function() {
  dir <- getOption(ACTIVE_RUN_DIR_OPTION, NULL)
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    return(NULL)
  }
  dir
}

#' Find the most recent run subdirectory of a base output directory
#'
#' @description
#' Run directories are named after the run's timestamp, so their names sort
#' chronologically. The sort is done in the C locale, which keeps a
#' collision suffix (`..._14-30-12-2`) after the directory it disambiguates.
#'
#' @param base_dir *\[character\]* The base output directory.
#' @return *\[character or NULL\]* The latest run directory, or `NULL` when the
#'   base directory holds no run subdirectories.
#' @keywords internal
latest_run_output_dir <- function(base_dir) {
  if (!is.character(base_dir) || length(base_dir) != 1L || is.na(base_dir)) {
    return(NULL)
  }
  runs_dir <- file.path(base_dir, RUNS_DIR_NAME)
  if (!dir.exists(runs_dir)) {
    return(NULL)
  }
  entries <- list.dirs(runs_dir, full.names = FALSE, recursive = FALSE)
  if (length(entries) == 0L) {
    return(NULL)
  }
  entries <- sort(entries, method = "radix")
  file.path(runs_dir, entries[[length(entries)]])
}

#' Start a new run directory
#'
#' @description
#' Called once per run, before any method writes a file. With
#' `output.run_subdirectories` off this is a no-op that returns the base output
#' directory, which is what every run has always written into. With the option
#' on it picks a fresh timestamped subdirectory and records it, so that every
#' path resolved for the rest of the run (graphics, tables, manifest, report)
#' lands in it. The directory itself is created by `ensure_output_dirs()`.
#'
#' @param base_dir *\[character, optional\]* The base output directory the run
#'   subdirectory is created in. Defaults to the resolved base directory.
#' @param time *\[POSIXct, optional\]* The run's timestamp.
#' @return *\[character\]* The directory this run writes into.
#' @keywords internal
begin_run_output_dir <- function(base_dir = resolve_base_output_dir(), time = Sys.time()) {
  if (!use_run_subdirectories()) {
    clear_run_output_dir()
    return(base_dir)
  }

  runs_dir <- file.path(base_dir, RUNS_DIR_NAME)
  stem <- format(time, "%Y-%m-%d_%H-%M-%S")
  run_dir <- file.path(runs_dir, stem)
  # Two runs started within the same second must not share a directory.
  suffix <- 1L
  while (dir.exists(run_dir)) {
    suffix <- suffix + 1L
    run_dir <- file.path(runs_dir, paste0(stem, "-", suffix))
  }

  options(stats::setNames(list(run_dir), ACTIVE_RUN_DIR_OPTION))
  run_dir
}

#' Forget the run directory of the run that just finished
#'
#' @return `NULL`, invisibly.
#' @keywords internal
clear_run_output_dir <- function() {
  options(stats::setNames(list(NULL), ACTIVE_RUN_DIR_OPTION))
  invisible(NULL)
}

#' Resolve the output directory path
#'
#' @description
#' The directory results are written to and read back from. While a run is
#' executing this is that run's own directory, so every path a method resolves
#' lands with the rest of the run's output. Outside a run it is the base
#' directory ([resolve_base_output_dir()]), except with
#' `output.run_subdirectories` enabled, where it is the latest run's
#' subdirectory so that `results_dir()` and `results_open()` land on the most
#' recent results rather than on the directory holding every run.
#'
#' @return *\[character\]* The resolved output directory path.
resolve_output_dir <- function() {
  active <- active_run_output_dir()
  if (!is.null(active)) {
    return(active)
  }

  base_dir <- resolve_base_output_dir()

  if (use_run_subdirectories()) {
    latest <- latest_run_output_dir(base_dir)
    if (!is.null(latest)) {
      return(latest)
    }
  }

  base_dir
}

#' Check whether a path is absolute
#'
#' @description
#' Recognises POSIX roots (`/foo`), Windows drive letters (`C:\\foo`, `C:/foo`)
#' and UNC paths (`\\\\server\\share`).
#'
#' @param path *\[character\]* The path to test.
#' @return *\[logical\]* `TRUE` when the path is absolute.
#' @keywords internal
is_absolute_path <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\])", path)
}

#' Resolve the graphics subdirectory path
#'
#' @description
#' Reads the `artma.visualization.export_path` option (default: `"graphics"`)
#' and resolves it relative to the given output directory. A `~` is expanded
#' first, so `~/figures` is treated as the absolute path it denotes. An absolute
#' `export_path` is returned as-is; joining it to `output_dir` would otherwise
#' produce a nested copy of the whole absolute path under the results directory.
#'
#' This is the single place graphics paths are resolved. Every caller that needs
#' the directory plots are written to must route through it, otherwise the
#' directory that gets created and the directory that gets written to drift
#' apart for absolute paths.
#'
#' @param output_dir *\[character\]* The base output directory.
#' @param export_path *\[character, optional\]* The configured export path.
#'   Defaults to the `artma.visualization.export_path` option.
#' @return *\[character\]* The resolved graphics directory path.
resolve_graphics_dir <- function(output_dir, export_path = NULL) {
  export_path <- export_path %||% getOption("artma.visualization.export_path", "graphics")
  if (length(export_path) == 1L && !is.na(export_path) && nzchar(export_path)) {
    export_path <- path.expand(export_path)
  }
  if (is_absolute_path(export_path)) {
    return(export_path)
  }
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
  box::use(artma / libs / infrastructure / output_files[record_output_file])

  tables_dir <- file.path(output_dir, "tables")

  if ("csv" %in% formats) {
    csv_path <- file.path(tables_dir, paste0(name, ".csv"))
    utils::write.csv(df, file = csv_path, row.names = FALSE)
    record_output_file(csv_path)
  }

  if ("tex" %in% formats) {
    box::use(artma / output / latex[write_latex_table])
    tex_path <- file.path(tables_dir, paste0(name, ".tex"))
    write_latex_table(
      df,
      path = tex_path,
      caption = table_caption(name),
      label = paste0("tab:", name)
    )
    record_output_file(tex_path)
  }

  invisible()
}

#' Export all results to the output directory
#'
#' @description
#' Iterates over a named list of method results and exports each method's
#' tabular data (the `estimates` and `tables` slots of the standard return
#' contract). Graphics are written by each method during execution, so
#' plot-only methods simply contribute no tables here.
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
#' A generic walk over the standard return contract. When the result carries an
#' `estimates` frame with at least one row, that frame is the machine-readable
#' artifact and takes the `<method_name>.csv` name: it is written unrounded,
#' exactly as the method produced it, and the display table that would otherwise
#' own that name moves to `<method_name>_display.csv`. An empty `estimates`
#' frame (a plot-only method) is treated as no estimates at all, so no
#' header-only CSV is written. LaTeX output is inherently presentational, so it
#' stays driven by the display tables under their original names and the
#' `estimates` frame is never written as `.tex`.
#'
#' Everything else (`plots`, `meta`) is ignored here. There are no per-method
#' branches.
#'
#' @param result The method's return value (a `list` with `tables` and
#'   `estimates` slots).
#' @param method_name *\[character\]* The method name.
#' @param output_dir *\[character\]* The base output directory.
#' @keywords internal
export_method_result <- function(result, method_name, output_dir) {
  if (!is.list(result)) {
    return(invisible())
  }

  formats <- resolve_table_formats()
  write_csv <- "csv" %in% formats
  write_tex <- "tex" %in% formats

  estimates <- result$estimates
  # A plot-only method contributes an empty frame (or none at all). Writing it
  # would leave a header-only CSV next to the graphics and push the display
  # table to a `_display` name for no reason, so treat it as absent.
  has_estimates <- is.data.frame(estimates) && nrow(estimates) > 0L
  if (has_estimates && write_csv) {
    save_table(estimates, method_name, output_dir, formats = "csv")
  }

  tables <- result$tables
  if (!is.list(tables) || length(tables) == 0L) {
    return(invisible())
  }

  table_names <- names(tables)
  for (i in seq_along(tables)) {
    tbl <- tables[[i]]
    if (!is.data.frame(tbl)) next
    key <- if (is.null(table_names)) NULL else table_names[[i]]
    table_basename <- resolve_table_basename(method_name, key)

    if (write_csv) {
      csv_basename <- if (has_estimates && identical(table_basename, method_name)) {
        paste0(method_name, "_display")
      } else {
        table_basename
      }
      save_table(tbl, csv_basename, output_dir, formats = "csv")
    }

    if (write_tex) {
      save_table(tbl, table_basename, output_dir, formats = "tex")
    }
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
  auto_output_subdir,
  is_auto_output_dir,
  resolve_base_output_dir,
  resolve_output_dir,
  resolve_graphics_dir,
  latest_run_output_dir,
  use_run_subdirectories,
  begin_run_output_dir,
  clear_run_output_dir,
  ensure_output_dirs,
  export_results
)
