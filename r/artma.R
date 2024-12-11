#' @export
artma <- function(config_file = NULL, args = commandArgs(trailingOnly = TRUE)) {
  box::use(
    . / src / options[load_options],
    . / src / const[CONST]
  )

  config_file <- config_file %||% glue::glue("modules/static/{CONST$OPTIONS_FILE_NAME}")

  # load_options(glue::glue("modules/static/{CONST$OPTIONS_FILE_NAME}"), args = args)
  # load_options(glue::glue("abc.{CONST$OPTIONS_FILE_NAME}"))
  # print(glue::glue("abc.{CONST$OPTIONS_FILE_NAME}"))
  # Usage
  print(config_file)
  # print(getOption("artma"))
}
