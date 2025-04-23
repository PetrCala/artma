#' @title Fix the data config
#' @description Fix the data config.
#' @param options_file_name *\[character, optional\]* The name of the options file to read the data config from. If `NULL` (default), the data config will be read from the `artma.data.config` option.
#' @param options_dir *\[character, optional\]* The directory to read the options file from. If `NULL` (default), the current working directory will be used.
#' @return *\[list\]* The fixed data config.
#' @export
config.fix <- function(options_file_name = NULL, options_dir = NULL) {
  already_valid_msg <- "The data config is already valid."
  success_msg <- "The data config has been fixed."

  fix <- function() {
    config <- getOption("artma.data.config")

    # Running options.load() allows the user to select the options file and directory if empty.
    # To ensure chosen paths are available here, we fetch them from the options() namespace.
    options_file_name <- getOption("artma.temp.file_name")
    options_dir <- getOption("artma.temp.dir_name")

    if (is.null(options_file_name) || is.null(options_dir)) {
      cli::cli_abort("There was an error loading the options file - the options file name and directory are not set.")
    }

    if (is.list(config)) {
      cli::cli_alert_success(already_valid_msg)
      return(invisible(NULL))
    }

    fix_auto <- function() {
      box::use(
        artma / data / read[read_data],
        artma / data_config / parse[parse_df_into_data_config]
      )
      df_path <- getOption("artma.data.source_path")
      df <- read_data(df_path)
      config <- parse_df_into_data_config(df)
      artma::options.modify(
        options_file_name = options_file_name,
        options_dir = options_dir,
        user_input = list(
          "artma.data.config" = config
        ),
        should_validate = TRUE
      )
      cli::cli_alert_success(success_msg)
      return(invisible(NULL))
    }

    config_setup <- getOption("artma.data.config_setup")

    switch(config_setup,
      "auto" = fix_auto(),
      "manual" = cli::cli_abort("Manual data config is not implemented yet.")
    )
    return(invisible(NULL))
  }
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = fix
  )
}
