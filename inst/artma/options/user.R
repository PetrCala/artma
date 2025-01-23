#' @title Create user options file
#' @param parsed_options [list] A list of parsed options from the template file
#' @param path [character] A path to create the
#' @export
create_user_options_file <- function(parsed_options, path) {
  # if (file.exists(path)) {
  #   logger::log_info(glue::glue("An options file already exists under the path {path}. Overwriting this file..."))
  # }
  box::use(
    artma / options / utils[flat_to_nested],
    artma / libs / file_utils[ensure_folder_existence]
  )

  nested_options <- flat_to_nested(parsed_options)

  logger::log_debug(glue::glue("Writing a user options file to {path}..."))

  ensure_folder_existence(dirname(path))
  yaml::write_yaml(
    nested_options,
    path
  )
}
#' Load options from a user options YAML file and command line arguments. Store the options in the global environment.
#' @param path [character] Full path to the YAML file containing the options.
#' @param args [vector(character)] Command line arguments to parse.
#' @export
load_user_options_file <- function(path, args) {
  options(prefixed_options)
}
