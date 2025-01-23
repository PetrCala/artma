#' This function is about creating a user options file from the template
#' options template - full path to the options template file
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
