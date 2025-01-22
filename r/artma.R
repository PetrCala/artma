#' @export
artma <- function(
    options_file_path = NULL,
    args = commandArgs(trailingOnly = TRUE)) {
  artma_setup()
  box::use(
    artma / options[load_options],
    artma / const[CONST],
  )

  options_file_path <- options_file_path %||% CONST$OPTIONS_FILE_PATH

  load_options(path = options_file_path, args = args)
  print("Options loaded.")
}
