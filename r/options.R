options.create <- function(
    options_template_file_path = NULL) {
  setup.ensure_valid_boxpath()
  options_template_file_path <- options_template_file_path %||% PATHS$FILE_OPTIONS_TEMPLATE
}
