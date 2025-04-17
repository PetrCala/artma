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
    artma / data / utils[determine_vector_type],
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
    col_name_clean <- make.names(col)
    col_name_verbose <- make_verbose_name(col)
    col_data <- df[[col]]

    col_data_type <- tryCatch(
      determine_vector_type(
        data = col_data,
        recognized_data_types = CONST$DATA_CONFIG$DATA_TYPES
      ),
      error = function(e) {
        cli::cli_alert_warning("Failed to determine the data type of the column {.val {col}}.")
        "unknown"
      }
    )

    col_config[[CONST$DATA_CONFIG$KEYS$VAR_NAME]] <- col
    col_config[[CONST$DATA_CONFIG$KEYS$VAR_NAME_VERBOSE]] <- col_name_verbose
    col_config[[CONST$DATA_CONFIG$KEYS$VAR_NAME_DESCRIPTION]] <- col_name_verbose
    col_config[[CONST$DATA_CONFIG$KEYS$DATA_TYPE]] <- col_data_type
    col_config[[CONST$DATA_CONFIG$KEYS$NA_HANDLING]] <- getOption(
      "artma.data.na_handling"
    )

    config[[col_name_clean]] <- col_config
  }

  # column_configs <- lapply(names(df), process_column, df = df)

  # config <- stats::setNames(
  #   lapply(column_configs, function(x) x$config),
  #   vapply(column_configs, function(x) x$name, character(1))
  # )

  config
}

box::export(
  construct_data_config_filename,
  parse_df_into_data_config
)
