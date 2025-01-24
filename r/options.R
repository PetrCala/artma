#' @title Create user options
#' @description Create a new user options file from an options template.
#' @param options_name [character] Name of the new user options
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param template_path [character, optional] Full path to the options template file.
#' @param args Command line arguments to pass to the template parsing operation. This allows overwriting of the template with custom option values.
#' @returns NULL Creates the options file.
#' @export
options.create <- function(
    options_name,
    options_dir = NULL,
    template_path = NULL,
    args = commandArgs(trailingOnly = TRUE)) {
  static_setup()

  if (grepl(".yaml$", options_name)) {
    rlang::abort(glue::glue("Please pass the options file name without the .yaml suffix."))
  }

  box::use(
    artma / paths[PATHS],
    artma / options / index[parse_options_from_template, create_user_options_file]
  )

  if (is.null(options_dir)) options_dir <- PATHS$DIR_USER_OPTIONS
  options_file_name <- paste0(options_name, ".yaml")
  options_file_path <- file.path(options_dir, options_file_name)

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  parsed_options <- parse_options_from_template(path = template_path, args = args)

  create_user_options_file(
    options_name = options_name,
    parsed_options = parsed_options,
    path = options_file_path
  )
}

#' @title
#' @export
options.apply <- function(
    options_name,
    options_dir = NULL) {
  static_setup()

  if (grepl(".yaml$", options_name)) {
    rlang::abort(glue::glue("Please pass the options file name without the .yaml suffix."))
  }

  box::use(
    artma / paths[PATHS],
    artma / options / index[apply_user_options_file]
  )

  if (is.null(options_dir)) options_dir <- PATHS$DIR_USER_OPTIONS

  options_file_name <- glue::glue("{options_name}.yaml")
  options_file_path <- file.path(options_dir, options_file_name)

  apply_user_options_file(path = options_file_path)

  logger::log_info(glue::glue("The following user options have been applied: '{options_name}'."))
}

# #' @export
# options.list <- function()
