# source(file.path(dirname(normalizePath(sys.frame(1)$ofile)), "config.R"))

current_box_path <- getOption("box.path", character(0))

new_path <- paste(find.package("artma"), "R", sep = "/") # ../site-library/artma/R/

if (!any(grepl("artma/R$", current_box_path))) {
  # Make the package available to the box
  options(
    box.path = c(current_box_path, new_path)
  )
}


#' @export
artma <- function(config_file = NULL, args = commandArgs(trailingOnly = TRUE)) {
  box::use(
    R / artma / options[load_options],
    R / artma / const[CONST]
  )

  config_file <- config_file %||% glue::glue("modules/static/{CONST$OPTIONS_FILE_NAME}")

  # load_options(glue::glue("modules/static/{CONST$OPTIONS_FILE_NAME}"), args = args)
  # load_options(glue::glue("abc.{CONST$OPTIONS_FILE_NAME}"))
  # print(glue::glue("abc.{CONST$OPTIONS_FILE_NAME}"))
  # Usage
  print("Hi!")
  print(config_file)
  # print(getOption("artma"))
}
