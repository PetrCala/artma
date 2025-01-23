#' This function is about creating a user options file from the template
#' options template - full path to the options template file
options.create <- function(
    options_template_path = NULL,
    args = commandArgs(trailingOnly = TRUE)) {
  static_setup()
  box::use(
    artma / paths[PATHS],
    artma / options / index[parse_options_from_template, create_user_options_file]
  )

  options_template_path <- options_template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  parsed_options <- parse_options_from_template(path = options_template_path, args = args)

  out_path <- file.path(PATHS$DIR_TEMP, "options", "some_file.yaml")

  create_user_options_file(
    parsed_options = parsed_options,
    path = out_path
  )
}
