#' Construct a data config filename from a dataframe name
#'
#' @param df_name *\[character\]* The name of the dataframe
#' @param should_validate *\[logical\]* Whether to validate the filename
#' @return *\[character\]* The filename of the data config
construct_data_config_filename <- function(df_name, should_validate = TRUE) {
  box::use(
    artma / const[CONST],
    artma / data_config / validate[data_config_filename_is_valid]
  )

  filename <- paste0(df_name, CONST$PATTERNS$DATA_CONFIG$PLAIN)
  if (should_validate && !data_config_filename_is_valid(filename)) {
    cli::cli_abort("The following data config filename is not valid: {.path {filename}}")
  }
  filename
}



#' Parse a dataframe into a data config
#'
#' @param df *\[data.frame\]* The dataframe to parse
#' @param df_name *\[character\]* The name of the dataframe
#' @param data_config_dir *\[character\]* The directory to save the data config
#' @return *\[list\]* The data config
parse_df_into_data_config <- function(df, df_name, data_config_dir = NULL) {
  box::use(
    artma / libs / validation[validate]
  )

  validate(
    is.data.frame(df),
    is.character(df_name)
  )

  config <- list()

  config
}

box::export(
  construct_data_config_filename,
  parse_df_into_data_config
)
