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
#' tabular data as CSV files. Graphics-only methods are skipped (they are
#' handled by per-method export during execution).
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
}

#' Export a single method's result
#'
#' @param result The method's return value.
#' @param method_name *\[character\]* The method name.
#' @param output_dir *\[character\]* The base output directory.
#' @keywords internal
export_method_result <- function(result, method_name, output_dir) {
  if (is.data.frame(result)) {
    save_table(result, method_name, output_dir)
    return(invisible())
  }

  if (!is.list(result)) return(invisible())

  # Methods with $summary
  if (!is.null(result$summary) && is.data.frame(result$summary)) {
    save_table(result$summary, method_name, output_dir)
    return(invisible())
  }

  # Methods with $coefficients (bma, fma)
  if (!is.null(result$coefficients) && is.data.frame(result$coefficients)) {
    save_table(result$coefficients, method_name, output_dir)
    return(invisible())
  }

  # p_hacking_tests: multiple sub-tables
  sub_tables <- c("caliper", "elliott", "maive")
  exported_any <- FALSE
  for (sub in sub_tables) {
    if (!is.null(result[[sub]]) && is.data.frame(result[[sub]])) {
      save_table(result[[sub]], paste0(method_name, "_", sub), output_dir)
      exported_any <- TRUE
    }
  }
  if (exported_any) return(invisible())

  invisible()
}

box::export(
  resolve_output_dir,
  resolve_graphics_dir,
  ensure_output_dirs,
  export_results
)
