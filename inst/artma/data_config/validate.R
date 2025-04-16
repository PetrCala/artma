#' @title Validate that a filename follows the data config file naming convention
#' @description Check if a filename ends with the data config file suffix
#' @param filename *\[character\]* The filename to validate
#' @return *\[logical\]* `TRUE` if the filename is valid, `FALSE` otherwise
data_config_filename_is_valid <- function(filename) {
  box::use(artma / const[CONST])

  if (rlang::is_empty(filename)) {
    return(FALSE)
  }

  grepl(CONST$REGEX$DATA_CONFIG_FILE_SUFFIX, filename)
}

#' @title Check if data config file exists
#' @description Check if a data configuration file exists for a given data frame name in the specified directory.
#' @note Provide the name of the data frame including the suffix.
#' @param df_name *\[character\]* Name of the data frame to check for a config file
#' @param data_config_dir *\[character, optional\]* Directory to search for the config file. If `NULL`, uses the default directory.
#' @return *\[logical\]* `TRUE` if a matching config file exists, `FALSE` otherwise
df_data_config_exists <- function(df_name, data_config_dir = NULL) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS]
  )

  df_name <- tools::file_path_sans_ext(df_name)

  data_config_dir <- if (is.null(data_config_dir)) PATHS$DIR_USR_DATA_CONFIGS else data_config_dir
  if (!dir.exists(data_config_dir)) {
    cli::cli_abort("The following data config directory does not exist: {.path {data_config_dir}}")
  }

  pattern <- paste0("^", df_name, CONST$REGEX$DATA_CONFIG_FILE_SUFFIX, "$")
  matching_files <- list.files(path = data_config_dir, pattern = pattern, full.names = TRUE)
  return(length(matching_files) > 0)
}

box::export(
  data_config_filename_is_valid,
  df_data_config_exists
)
