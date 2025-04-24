#' @title Get data config
#' @description Get a data config from the user options file. If it does not exist, it will be created from the dataframe.
#' @param create_if_missing *\[logical\]* Whether to create the data config if it does not exist. Defaults to `TRUE`.
#' @param fix_if_invalid *\[logical\]* Whether to fix the data config if it is invalid. Defaults to `FALSE`.
#' @return *\[list\]* The data config.
get_data_config <- function(
    create_if_missing = TRUE,
    fix_if_invalid = FALSE) {
  box::use(
    artma / libs / validation[validate],
    artma / data_config / utils[data_config_is_valid],
    artma / data_config / write[fix_data_config]
  )

  validate(
    is.logical(create_if_missing),
    is.logical(fix_if_invalid)
  )

  config <- getOption("artma.data.config")

  config_exists <- !is.na(config) && !is.null(config)

  if (isTRUE(config_exists)) {
    if (data_config_is_valid(config)) {
      return(config)
    }

    if (!fix_if_invalid) {
      cli::cli_abort("The data config is invalid. Please run {.code artma::config.fix()} to fix it.")
    }
  }

  # The config does not exist yet - we can safely create it
  cli::cli_inform("Creating a new data config.")
  config <- suppressMessages(fix_data_config(create_if_missing = create_if_missing))
  return(config)
}

box::export(get_data_config)
