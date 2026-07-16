#' @title Legacy options file migration
#' @description Converts options files that predate the unified per-column
#'   store. Old files kept column semantics in two places: the name mapping
#'   under `data.colnames.*` and the per-variable analysis config under
#'   `data.config`. Both are folded into one `data.columns` record per column.
#'   Migration runs from the runtime setup and `options.fix()` paths; loading
#'   itself stays pure and never rewrites files.

#' @title Detect the legacy dual-store format
#' @description Checks whether a nested options list (as read from a YAML
#'   options file) still uses the legacy `data.colnames` / `data.config` stores.
#' @param nested_options *\[list\]* The nested options list.
#' @return *\[logical\]* `TRUE` when either legacy store is present.
is_legacy_options_format <- function(nested_options) {
  if (!is.list(nested_options) || !is.list(nested_options[["data"]])) {
    return(FALSE)
  }
  data_node <- nested_options[["data"]]
  !is.null(data_node[["colnames"]]) || !is.null(data_node[["config"]])
}

#' @title Build unified column records from the legacy stores
#' @description Merges the legacy name mapping and per-variable config into
#'   unified per-column records. Role records (keyed by the standard name)
#'   carry the mapped `source_name`; identity mappings are dropped since the
#'   sparse store does not need them.
#' @param colnames_map *\[list\]* The legacy `data.colnames` mapping
#'   (standard name -> source column name).
#' @param config *\[list\]* The legacy `data.config` per-variable entries.
#' @return *\[list\]* The unified per-column records.
build_unified_columns <- function(colnames_map, config) {
  records <- list()

  if (is.list(config)) {
    for (key in names(config)) {
      entry <- config[[key]]
      if (is.list(entry)) {
        records[[key]] <- entry
      }
    }
  }

  if (is.list(colnames_map)) {
    for (std in names(colnames_map)) {
      src <- colnames_map[[std]]
      if (is.null(src) || length(src) != 1 || is.na(src) || !nzchar(src)) next
      if (identical(as.character(src), std)) next # identity mapping: not stored
      entry <- records[[std]]
      if (!is.list(entry)) entry <- list()
      entry$source_name <- as.character(src)
      records[[std]] <- entry
    }
  }

  records
}

#' @title Migrate a legacy options file to the unified column store
#' @description Detects the legacy dual-store format and rewrites the file in
#'   place: `data.colnames.*` and `data.config` are replaced by the unified
#'   `data.columns` store. A no-op for files already in the current format.
#' @param options_file_name *\[character\]* Name of the options file, including
#'   the suffix.
#' @param options_dir *\[character, optional\]* Directory containing user
#'   options files. Defaults to the standard directory.
#' @return *\[logical\]* Invisibly, whether a migration was performed.
migrate_legacy_options <- function(options_file_name, options_dir = NULL) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / options / files[
      options_file_path,
      read_options_file,
      resolve_options_dir,
      write_options_file
    ]
  )

  options_dir <- resolve_options_dir(options_dir, must_exist = FALSE)
  options_path <- options_file_path(options_dir, options_file_name)

  if (!file.exists(options_path)) {
    return(invisible(FALSE))
  }

  nested <- read_options_file(options_path)
  if (!is_legacy_options_format(nested)) {
    return(invisible(FALSE))
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info(
      "Your options file {.file {options_file_name}} uses the legacy column format ({.code data.colnames} + {.code data.config}). Migrating it to the unified {.code data.columns} store."
    )
  }

  records <- build_unified_columns(
    colnames_map = nested[["data"]][["colnames"]],
    config = nested[["data"]][["config"]]
  )

  nested[["data"]][["colnames"]] <- NULL
  nested[["data"]][["config"]] <- NULL
  nested[["data"]][["columns"]] <- if (length(records) > 0) records else NA

  write_options_file(options_path, nested)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success(
      "Migrated {.file {options_file_name}} to the unified per-column format ({length(records)} column record{?s})."
    )
  }

  invisible(TRUE)
}

box::export(
  build_unified_columns,
  is_legacy_options_format,
  migrate_legacy_options
)
