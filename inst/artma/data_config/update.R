#' @title Update a data config file
#' @description Update a data config file with a new dataframe. If the data config file does not exist, it will be created.
#' @param data_config *\[list\]* The data config to use, provided as a list.
#' @param df_name *\[character\]* The name of the dataframe for which the data config is being updated.
#' @param data_config_dir *\[character, optional\]* The directory to save the data config. If `NULL`, uses the default directory.
#' @return *\[NULL\]* The function is called for its side effects
update_data_config_file <- function(data_config, df_name, data_config_dir = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / libs / ask[ask_for_overwrite_permission],
    artma / libs / file_utils[ensure_folder_existence],
    artma / libs / validation[validate],
    artma / data_config / parse[construct_data_config_filename]
  )

  validate(
    is.list(data_config),
    is.character(df_name)
  )

  data_config_dir <- if (is.null(data_config_dir)) PATHS$DIR_USR_DATA_CONFIGS else data_config_dir
  ensure_folder_existence(data_config_dir)

  filename <- construct_data_config_filename(df_name)
  file_path <- file.path(data_config_dir, filename)

  ask_for_overwrite_permission(file_path, action_name = "updating a data config file")
  cli::cli_inform("Writing data config file to {.path {file_path}}")
  jsonlite::write_json(
    data_config,
    file_path
  )
  return(invisible(NULL))
}

box::export(
  update_data_config_file
)
