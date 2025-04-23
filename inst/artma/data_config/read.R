#' @title Get data config
#' @description Get a data config from the user options file. If it does not exist, it will be created from the dataframe.
#' @param df *\[data.frame, optional\]* The dataframe to create the data config from. If `NULL` (default), the data config will be read from the `artma.data.config` option.
#' @return *\[list\]* The data config.
get_data_config <- function(
    df = NULL) {
  box::use(
    artma / const[CONST],
    artma / data_config / parse[parse_df_into_data_config]
  )

  err_msg <- "The data config is of a malformed value. Please run {.code artma::config.fix()} to fix it."

  config <- getOption("artma.data.config")

  if (is.list(config)) {
    return(config)
  }
  if (!(is.na(config) || is.null(config))) cli::cli_abort(err_msg)

  # The config does not create yet - we need to create it
  if (is.null(df)) {
    cli::cli_abort("The data config is not created yet. Please provide a dataframe to create it.")
  }

  config_setup <- getOption("artma.data.config_setup")
  if (!(config_setup %in% CONST$DATA_CONFIG$SETUP_TYPES)) {
    cli::cli_abort("Invalid data config setup type. Must be one of: {.val {CONST$DATA_CONFIG$SETUP_TYPES}}")
  }
  config <- switch(config_setup,
    "auto" = parse_df_into_data_config(df),
    # In the manual case, stop in non-interactive mode and in interactive, prompt the user to select the data config file.
    "manual" = cli::cli_abort("Manual data config is not implemented yet."), # read_data_config_from_file(),
    cli::cli_abort(err_msg)
  )
}

box::export(get_data_config)
