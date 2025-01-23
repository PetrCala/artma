#' @title Create user options file
#' @param parsed_options [list] A list of parsed options from the template file
#' @param path [character] A path to create the
#' @export
create_user_options_file <- function(parsed_options, path) {
  if (!is.list(parsed_options)) {
    rlang::abort("Invalid parsed options - must be a list.")
  }
  if (!is.character(path) || length(path) <= 0) {
    rlang::abort(glue::glue("Invalid path: {path}"))
  }
  if (!grepl(".yaml$", path)) {
    rlang::abort(glue::glue("The user options file must contain the '.yaml' suffix. Got: {path}."))
  }
  if (file.exists(path)) {
    logger::log_info(glue::glue("An options file already exists under the path {path}. Overwriting this file..."))
  }

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

#' Load options from a user options YAML file. Store the options in the global environment.
#' @param path [character] Full path to the YAML file containing the user options.
#' @export
load_user_options_file <- function(path) {
  if (!is.character(path) || length(path) <= 0) {
    rlang::abort(glue::glue("Invalid path: {path}"))
  }
  if (!file.exists(path)) {
    logger::abort(glue::glue("The following user options file does not exist: {path}.\nYou can create a user options file using 'artma::options.create'."))
  }

  box::use(
    artma / const[CONST],
    artma / options / utils[nested_to_flat],
  )

  nested_options <- yaml::read_yaml(path)

  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = CONST$PACKAGE_NAME)

  options_file_name <- prefixed_options[["artma.general.name"]]
  logger::log_info(glue::glue("Applying the following options: '{options_file_name}'"))

  options(prefixed_options)
}
