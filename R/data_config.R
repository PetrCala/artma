#' @title Fix the data config
#' @description Fix the data config.
#' @param options_file_name *\[character, optional\]* The name of the options file to read the data config from. If `NULL` (default), the data config will be read from the `artma.data.config` option.
#' @param options_dir *\[character, optional\]* The directory to read the options file from. If `NULL` (default), the current working directory will be used.
#' @return *\[list\]* The fixed data config.
#' @export
config.fix <- function(options_file_name = NULL, options_dir = NULL) {
  # config <- if (is.null(config)) getOption("artma.data.config") else config
  # Run options.fix potentially
  # if (is.list(config)) {
  #   return(config)
  # }
  # return(list())
  cli::cli_abort("Not implemented yet.")
}
