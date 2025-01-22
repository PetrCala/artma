#' @title ARTMA Setup
#'
#' @description
#' A function to be called at the beginning of each exported function function to ensure crucial fucntionality, such as imports, logging, etc., all work as expected.
#' @keywords internal
artma_setup <- function(
    options_file_path = NULL,
    args = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options[load_options],
    artma / libs / logs / index[setup_logging]
  )

  options_file_path <- options_file_path %||% PATHS$FILE_OPTIONS

  load_options(path = options_file_path, args = args)
  setup_logging()
}
