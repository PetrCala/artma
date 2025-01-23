#' options template - full path to the options template file
options.create <- function(
    options_template = NULL) {
  static_setup()
  box::use(
    artma / paths[PATHS]
  )

  options_template <- options_template %||% PATHS$FILE_OPTIONS_TEMPLATE
  load_options(path = options_file_path, args = args)
}
