#' @title Update Data Config
#' @description Update the data config.
#' @param changes *\[list\]* The changes to the data config.
#' @return *\[list\]* The updated data config.
update_data_config <- function(changes) {
  current_config <- getOption("artma.data.config")
  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")

  if (is.null(options_file_name) || is.null(options_dir)) {
    cli::cli_abort("There was an error loading the options file - the options file name and directory are not set.")
  }

  box::use(artma / libs / validation[validate])
  validate(is.list(changes))

  if (is.null(changes)) changes <- list()
  # The config can be not yet created or malformed. We want to reinstantiate it.
  if (!is.list(current_config)) current_config <- list()

  new_config <- utils::modifyList(current_config, changes)

  suppressMessages(
    artma::options.modify(
      options_file_name = options_file_name,
      options_dir = options_dir,
      user_input = list(
        "data.config" = new_config
      ),
      should_validate = TRUE
    )
  )

  # Ensure any further calls to getOption("artma.data.config") will return the fixed config
  # This option is reverted upon exiting from the namespace modified by runtime_setup()
  options("artma.data.config" = new_config)

  return(invisible(new_config))
}

#' @title Fix Data Config
#' @description Fix the data config.
#' @param create_if_missing *\[logical\]* Whether to create the data config if it does not exist. Defaults to `TRUE`.
#' @return *\[list\]* The fixed data config.
fix_data_config <- function(
    create_if_missing = TRUE) {
  box::use(artma / data_config / utils[data_config_is_valid])

  current_config <- getOption("artma.data.config")

  if (data_config_is_valid(current_config)) {
    cli::cli_alert_success("The data config is valid.")
    return(invisible(NULL))
  }

  if ((is.na(current_config) || is.null(current_config)) && !create_if_missing) {
    cli::cli_abort("The data config has not been created yet.")
  }

  cli::cli_inform("Creating a new data config...")

  box::use(
    artma / const[CONST],
    artma / data / read[read_data],
    artma / data_config / parse[parse_df_into_data_config]
  )

  if (!is.list(current_config)) current_config <- list()

  # TODO this should be automatically detected - for now, overwrite the whole config
  # invalid_config <- current_config

  # TODO this should check the invalid setup and overwrite it

  # For automatic options, overwrite the whole config (its invalid/missing values)
  # onto values that are valid
  # For manual options, only suggest these changes and ask for confirmation

  df_path <- getOption("artma.data.source_path")
  df <- read_data(df_path)

  config_setup <- getOption("artma.data.config_setup")
  if (!(config_setup %in% CONST$DATA_CONFIG$SETUP_TYPES)) {
    cli::cli_abort("Invalid data config setup type. Must be one of: {.val {CONST$DATA_CONFIG$SETUP_TYPES}}")
  }
  base_config <- parse_df_into_data_config(df)
  config <- switch(config_setup,
    "auto" = base_config,
    # In the manual case, stop in non-interactive mode and in interactive, prompt the user to select the data config file.
    "manual" = cli::cli_abort("Manual data config is not implemented yet."), # read_data_config_from_file(),
    cli::cli_abort(err_msg)
  )

  fixed_config <- suppressMessages(update_data_config(changes = config))

  cli::cli_alert_success("The data config has been fixed.")
  return(fixed_config)
}

box::export(
  fix_data_config,
  update_data_config
)
