#' @export
artma <- function(config_file = NULL, args = commandArgs(trailingOnly = TRUE)) {
  ensure_valid_boxpath()
  box::use(
    artma / options[load_options],
    artma / const[CONST],
  )

  config_file <- config_file %||% glue::glue("static/{CONST$OPTIONS_FILE_NAME}")

  load_options(CONST$OPTIONS_FILE_PATH, args = args)
}
