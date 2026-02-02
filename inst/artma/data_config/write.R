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
    artma / data_config / resolve[read_df_for_config]
  )

  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")

  if (is.null(options_file_name) || is.null(options_dir)) {
    cli::cli_abort(
      paste(
        "There was an error loading the options file -",
        "the options file name and directory are not set."
      )
    )
  }

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

  # Save sparse overrides to options file
  suppressMessages(
    artma::options.modify(
      options_file_name = options_file_name,
      options_dir = options_dir,
      user_input = list(
        "data.config" = sparse_overrides
      ),
      should_validate = TRUE
    )
  )

  # Update in-memory state
  options("artma.data.config" = sparse_overrides)

  invisible(new_resolved)
}

#' @title Fix Data Config
#' @description Fix the data config by regenerating from the dataframe.
#'   Since the base config IS the default, fixing means clearing all overrides.
#' @param create_if_missing *\[logical\]* Whether to create the data config if
#'   it does not exist. Defaults to `TRUE`.
#' @return *\[list\]* The fixed data config.
fix_data_config <- function(
    create_if_missing = TRUE) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data_config / defaults[build_base_config],
    artma / data_config / resolve[read_df_for_config]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Regenerating data config from dataframe...")
  }

  df <- read_df_for_config()
  base_config <- build_base_config(df)

  # Clear all overrides -- the base config is the canonical default
  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")

  if (!is.null(options_file_name) && !is.null(options_dir)) {
    suppressMessages(
      artma::options.modify(
        options_file_name = options_file_name,
        options_dir = options_dir,
        user_input = list("data.config" = list()),
        should_validate = TRUE
      )
    )
  }

  options("artma.data.config" = list())

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
  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")

  if (is.null(options_file_name) || is.null(options_dir)) {
    cli::cli_abort(
      paste(
        "There was an error loading the options file -",
        "the options file name and directory are not set."
      )
    )
  }

  current_overrides <- getOption("artma.data.config")
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

  suppressMessages(
    artma::options.modify(
      options_file_name = options_file_name,
      options_dir = options_dir,
      user_input = list("data.config" = new_overrides),
      should_validate = TRUE
    )
  )

  options("artma.data.config" = new_overrides)

  # Return the resolved config
  box::use(artma / data_config / read[get_data_config])
  invisible(get_data_config())
}

box::export(
  fix_data_config,
  update_data_config,
  reset_config_overrides
)
