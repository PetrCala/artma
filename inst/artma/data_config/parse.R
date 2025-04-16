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


#' Write a data config to a file
#'
#' @param config *\[list\]* The data config
#' @param file_path *\[character\]* The path to the file to write the data config to
#' @return *\[NULL\]* The function is called for its side effects
write_data_config <- function(config, df_name, data_config_dir = NULL) {
  box::use(
    artma / libs / ask[ask_for_overwrite_permission],
    artma / data_config / validate[data_config_filename_is_valid]
  )

  filename <- construct_data_config_filename(df_name)
  file_path <- file.path(data_config_dir, filename)

  ask_for_overwrite_permission(file_path, action_name = "updating a data config file")
  cli::cli_inform("Writing data config file to {.path {file_path}}")
  jsonlite::write_json(
    config,
    file_path
  )
}


#' Parse a dataframe into a data config
#'
#' @param df *\[data.frame\]* The dataframe to parse
#' @param df_name *\[character\]* The name of the dataframe
#' @param data_config_dir *\[character\]* The directory to save the data config
#' @param should_write *\[logical\]* Whether to write the data config to a file
#' @return *\[list\]* The data config
parse_df_into_data_config <- function(df, df_name, data_config_dir = NULL, should_write = FALSE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / libs / validation[validate],
    artma / libs / file_utils[ensure_folder_existence]
  )

  validate(
    is.data.frame(df),
    is.character(df_name)
  )

  data_config_dir <- if (is.null(data_config_dir)) PATHS$DIR_USR_DATA_CONFIGS else data_config_dir
  ensure_folder_existence(data_config_dir)

  config <- list()

  if (should_write) write_data_config(config, df_name, data_config_dir)

  config
}

box::export(
  construct_data_config_filename,
  parse_df_into_data_config,
  write_data_config
)
