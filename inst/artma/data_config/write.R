#' @title Write the unified per-column store
#' @description The single persistence path for the unified per-column store
#'   (`data.columns`). Replaces the store wholesale, both in memory and in the
#'   options file (when one is loaded), so removals are honored rather than
#'   merged over.
#' @param store *\[list\]* The full sparse store to persist.
#' @return `NULL`, invisibly.
write_unified_columns <- function(store) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / data / utils[get_standardized_colnames],
    artma / options / files[
      options_file_path,
      read_options_file,
      resolve_options_dir,
      write_options_file
    ]
  )

  validate(is.list(store))

  # Normalize: identity mappings are never stored (a role record whose
  # source_name equals its own key carries no information), and records left
  # empty by that are dropped entirely.
  for (key in intersect(names(store), get_standardized_colnames())) {
    entry <- store[[key]]
    if (is.list(entry) && identical(entry[["source_name"]], key)) {
      entry[["source_name"]] <- NULL
      store[[key]] <- if (length(entry) == 0) NULL else entry
    }
  }

  options("artma.data.columns" = store)

  options_file_name <- getOption("artma.temp.file_name", NULL)
  options_dir <- getOption("artma.temp.dir_name", NULL)
  if (is.null(options_file_name) || is.null(options_dir)) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      options_path <- options_file_path(resolve_options_dir(options_dir), options_file_name)
      nested <- read_options_file(options_path)
      nested[["data"]][["columns"]] <- store
      write_options_file(options_path, nested)
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Could not persist the column configuration to the options file: {e$message}"
        )
      }
    }
  )

  invisible(NULL)
}

#' @title Update Data Config
#' @description Update the data config with new changes. Changes are applied to
#'   the fully-resolved config, then diffed against the base defaults to produce
#'   sparse overrides that are saved to the options file.
#'   When the dataframe source is not available, changes are merged directly into
#'   existing overrides without diffing.
#' @param changes *\[list\]* The changes to the data config. A named list where
#'   each key is a variable name and each value is a list of field overrides.
#' @return *\[list\]* The updated fully-resolved data config (invisibly).
update_data_config <- function(changes) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / data_config / defaults[build_base_config, extract_overrides],
    artma / data_config / read[get_data_config],
    artma / data_config / resolve[read_df_for_config],
    artma / options / index[require_option]
  )

  require_option("artma.temp.file_name")
  require_option("artma.temp.dir_name")

  validate(is.list(changes))
  if (is.null(changes)) changes <- list()

  # Get the current resolved config and apply changes
  current_resolved <- get_data_config()
  new_resolved <- utils::modifyList(current_resolved, changes)

  # Try to build base config from df for sparse diffing
  df <- tryCatch(
    read_df_for_config(),
    error = function(e) {
      if (get_verbosity() >= 4) {
        cli::cli_inform(
          "No dataframe for sparse diffing: {e$message}"
        )
      }
      NULL
    }
  )

  if (!is.null(df)) {
    base_config <- build_base_config(df)

    # Compute sparse overrides: only non-default fields
    sparse_overrides <- list()
    for (var_key in names(new_resolved)) {
      default_entry <- base_config[[var_key]]

      if (is.null(default_entry)) {
        # Variable not in dataframe (e.g., computed column)
        sparse_overrides[[var_key]] <- new_resolved[[var_key]]
      } else {
        override <- extract_overrides(
          new_resolved[[var_key]], default_entry
        )
        if (!is.null(override)) {
          sparse_overrides[[var_key]] <- override
        }
      }
    }
  } else {
    # No df available -- store changes as-is (no diff possible)
    sparse_overrides <- new_resolved
  }

  write_unified_columns(sparse_overrides)

  invisible(new_resolved)
}

#' @title Fix Data Config
#' @description Fix the data config by regenerating from the dataframe.
#'   Since the base config IS the default, fixing means clearing all overrides
#'   except the column name mappings, which are not derivable from the data.
#' @param create_if_missing *\[logical\]* Whether to create the data config if
#'   it does not exist. Defaults to `TRUE`.
#' @return *\[list\]* The fixed data config.
fix_data_config <- function(
  create_if_missing = TRUE
) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / utils[get_colnames_map],
    artma / data_config / defaults[build_base_config],
    artma / data_config / resolve[read_df_for_config]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Regenerating data config from dataframe...")
  }

  # Preserve the column name mappings: they cannot be re-derived from the
  # dataframe and dropping them would orphan the source columns.
  colnames_map <- get_colnames_map()

  df <- read_df_for_config()
  base_config <- build_base_config(df)

  # Clear all analysis overrides -- the base config is the canonical default
  new_store <- list()
  for (std in names(colnames_map)) {
    new_store[[std]] <- list(source_name = colnames_map[[std]])
  }

  write_unified_columns(new_store)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success(
      "The data config has been regenerated from the dataframe."
    )
  }

  base_config
}

#' @title Reset Config Overrides
#' @description Remove overrides for a specific variable or all variables,
#'   resetting them to defaults.
#' @param var_name *\[character|NULL\]* The variable name to reset. If NULL,
#'   resets all overrides.
#' @return *\[list\]* The updated fully-resolved data config (invisibly).
reset_config_overrides <- function(var_name = NULL) {
  box::use(artma / options / index[require_option])

  require_option("artma.temp.file_name")
  require_option("artma.temp.dir_name")

  current_overrides <- getOption("artma.data.columns", list())
  if (!is.list(current_overrides)) current_overrides <- list()

  if (is.null(var_name)) {
    # Reset all overrides
    new_overrides <- list()
  } else {
    # Reset specific variable
    var_key <- make.names(var_name)
    new_overrides <- current_overrides
    new_overrides[[var_key]] <- NULL
  }

  write_unified_columns(new_overrides)

  # Return the resolved config
  box::use(artma / data_config / read[get_data_config])
  invisible(get_data_config())
}

box::export(
  fix_data_config,
  update_data_config,
  reset_config_overrides,
  write_unified_columns
)
