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
#' @return *\[list\]* The data config
parse_df_into_data_config <- function(df) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[validate],
    artma / libs / string[make_verbose_name]
  )

  validate(is.data.frame(df))

  if (nrow(df) == 0) {
    cli::cli_abort("The dataframe is empty. Please provide a dataframe with at least one row.")
  }

  config <- list()

  for (col in names(df)) {
    col_config <- list()
    col_name_clean <- make.names(col) # remove special characters
    # col_data <- df[[col]]

    col_config[[CONST$DATA_CONFIG$KEYS$VAR_NAME]] <- col
    col_config[[CONST$DATA_CONFIG$KEYS$VAR_NAME_VERBOSE]] <- make_verbose_name(col)

    config[[col_name_clean]] <- col_config
  }

  config
}

box::export(
  construct_data_config_filename,
  parse_df_into_data_config
)
