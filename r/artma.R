#' @export
artma <- function(config_file = NULL, args = commandArgs(trailingOnly = TRUE)) {
  ensure_valid_boxpath()
  box::use(
    artma / options[load_options],
    artma / const[CONST],
  )

  config_file <- config_file %||% glue::glue("static/{CONST$OPTIONS_FILE_NAME}")
  print("Package path:")
  print(CONST$PACKAGE_PATH)

  # load_options(glue::glue("modules/static/{CONST$OPTIONS_FILE_NAME}"), args = args)
  # load_options(glue::glue("abc.{CONST$OPTIONS_FILE_NAME}"))
  # print(glue::glue("abc.{CONST$OPTIONS_FILE_NAME}"))
  # # Usage
  # print(getOption("artma"))
}
