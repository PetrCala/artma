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
    artma / const[CONST],
    artma / paths[PATHS],
    artma / libs / validation[validate],
    artma / libs / file_utils[ensure_folder_existence],
  )

  validate(
    is.data.frame(df),
    is.character(df_name)
  )

  data_config_dir <- if (is.null(data_config_dir)) PATHS$DIR_USR_DATA_CONFIGS else data_config_dir
  ensure_folder_existence(data_config_dir)

  filename <- construct_data_config_filename(df_name)

  browser()

  # # Create a path to the file
  # file_path <- file.path(data_config_dir, filename)

  # # Create the file
  # file.create(file_path)

  # # Write the dataframe to the file
  # write.csv(df, file_path, row.names = FALSE)

  # # Return the data config
  # list(
  #   filename = filename,
  #   file_path = file_path
  # )
}

box::export(
  construct_data_config_filename,
  parse_df_into_data_config
)
