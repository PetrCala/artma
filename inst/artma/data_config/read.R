#' @title Get data config
#' @description Get a data config from the user options file. If it does not exist, it will be created from the dataframe.
#' @param create_if_missing *\[logical\]* Whether to create the data config if it does not exist. Defaults to `TRUE`.
#' @param fix_if_invalid *\[logical\]* Whether to fix the data config if it is invalid. Defaults to `FALSE`.
#' @return *\[list\]* The data config.
get_data_config <- function(
    create_if_missing = TRUE,
    fix_if_invalid = FALSE) {
  box::use(
    artma / const[CONST],
    artma / data_config / parse[parse_df_into_data_config],
    artma / data_config / utils[data_config_is_valid],
    artma / data_config / write[fix_data_config]
  )

  config <- getOption("artma.data.config")

  if (data_config_is_valid(config)) {
    return(config)
  }

  if (!fix_if_invalid) {
    cli::cli_abort("The data config is invalid. Please run {.code artma::config.fix()} to fix it.")
  }

  # The config does not create yet - we need to create it
  config <- fix_data_config(create_if_missing = create_if_missing)
  return(config)
}

box::export(get_data_config)
