#' @title Get the resolved data config
#' @description Returns the fully-resolved data config (base defaults merged
#'   with sparse overrides). If `var_name` is provided, returns only that
#'   variable's config entry.
#' @param var_name *\[character, optional\]* A specific variable name to
#'   retrieve. If `NULL` (default), returns the entire config.
#' @param options_file_name *\[character, optional\]* The name of the options
#'   file. If `NULL` (default), the user will be prompted interactively.
#' @param options_dir *\[character, optional\]* The directory containing options
#'   files. If `NULL` (default), the default directory is used.
#' @return *\[list\]* The fully-resolved data config (or a single entry).
#' @export
config.get <- function(var_name = NULL, options_file_name = NULL, options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = function() {
      box::use(artma / data_config / read[get_data_config])
      config <- get_data_config()
      if (!is.null(var_name)) {
        key <- make.names(var_name)
        if (!key %in% names(config)) {
          cli::cli_abort("Variable {.val {var_name}} not found in data config.")
        }
        return(config[[key]])
      }
      config
    }
  )
}

#' @title Set per-variable config overrides
#' @description Sets specific config fields for a variable. Only non-default
#'   values are persisted to the options file.
#' @param var_name *\[character\]* The variable name to configure.
#' @param ... Named arguments for config fields to set
#'   (e.g., `bma = TRUE, bma_to_log = TRUE`).
#' @param options_file_name *\[character, optional\]* The name of the options
#'   file. If `NULL` (default), the user will be prompted interactively.
#' @param options_dir *\[character, optional\]* The directory containing options
#'   files. If `NULL` (default), the default directory is used.
#' @return *\[list\]* The updated fully-resolved data config (invisibly).
#' @export
config.set <- function(var_name, ..., options_file_name = NULL, options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = function() {
      box::use(artma / data_config / write[update_data_config])
      fields <- list(...)
      if (length(fields) == 0) {
        cli::cli_abort("No config fields provided. Use named arguments, e.g., {.code bma = TRUE}.")
      }
      changes <- list()
      changes[[make.names(var_name)]] <- fields
      update_data_config(changes)
    }
  )
}

#' @title Reset variable config to defaults
#' @description Removes all overrides for a specific variable (or all
#'   variables), resetting them to auto-detected defaults.
#' @param var_name *\[character, optional\]* The variable name to reset. If
#'   `NULL` (default), resets all overrides.
#' @param options_file_name *\[character, optional\]* The name of the options
#'   file. If `NULL` (default), the user will be prompted interactively.
#' @param options_dir *\[character, optional\]* The directory containing options
#'   files. If `NULL` (default), the default directory is used.
#' @return *\[list\]* The updated fully-resolved data config (invisibly).
#' @export
config.reset <- function(var_name = NULL, options_file_name = NULL, options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = function() {
      box::use(artma / data_config / write[reset_config_overrides])
      reset_config_overrides(var_name)
    }
  )
}

#' @title View sparse config overrides
#' @description Returns only the sparse overrides that are actually persisted
#'   in the options file -- i.e., only non-default field values.
#' @param options_file_name *\[character, optional\]* The name of the options
#'   file. If `NULL` (default), the user will be prompted interactively.
#' @param options_dir *\[character, optional\]* The directory containing options
#'   files. If `NULL` (default), the default directory is used.
#' @return *\[list\]* The sparse overrides (only non-default values).
#' @export
config.overrides <- function(options_file_name = NULL, options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = function() {
      overrides <- getOption("artma.data.config")
      if (!is.list(overrides)) return(list())
      overrides
    }
  )
}

#' @title Fix the data config
#' @description Regenerate the data config from the dataframe, clearing all
#'   overrides.
#' @param options_file_name *\[character, optional\]* The name of the options
#'   file. If `NULL` (default), the user will be prompted interactively.
#' @param options_dir *\[character, optional\]* The directory containing options
#'   files. If `NULL` (default), the default directory is used.
#' @return *\[list\]* The fixed data config.
#' @export
config.fix <- function(options_file_name = NULL, options_dir = NULL) {
  box::use(artma / data_config / write[fix_data_config])
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = fix_data_config
  )
}
