#' @title Create user options file
#' @param options_name [character] Name of the options file. Is used only for logging purposes.
#' @param parsed_options [list] A list of parsed options from the template file
#' @param path [character] A path to create the options file.
#' @export
create_user_options_file <- function(options_name, parsed_options, path) {
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

  logger::log_info(glue::glue("Creating a new user options file: '{options_name}'"))

  ensure_folder_existence(dirname(path))
  yaml::write_yaml(
    nested_options,
    path
  )

  logger::log_success("User options file created.")
}

#' Load options from a user options YAML file. Store the options in the global environment.
#' @param path [character] Full path to the YAML file containing the user options.
#' @param should_return [logical, optional] Whether or not the function should return the list of options. Defaults to FALSE.
#' @keywords internal
load_user_options_file <- function(path, should_return = FALSE) {
  if (!is.character(path) || length(path) <= 0) {
    rlang::abort(glue::glue("Invalid path: {path}"))
  }
  if (!file.exists(path)) {
    rlang::abort(glue::glue("The following user options file does not exist: {path}.\nYou can create a user options file using 'artma::options.create'."))
  }

  box::use(
    artma / const[CONST],
    artma / options / utils[nested_to_flat],
  )

  nested_options <- yaml::read_yaml(path)

  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = CONST$PACKAGE_NAME)

  # Here, there should possibly be options validation functions

  options_file_name <- prefixed_options[["artma.general.name"]]
  logger::log_info(glue::glue("Applying the following options: '{options_file_name}'"))

  options(prefixed_options)

  if (should_return) {
    return(prefixed_options)
  }

  return(invisible(NULL))
}

#' @title Load user options
#' @description Load user options by their name.
#' @details In case the options name is not passed, the function will attempt to load the current options configuration. If none is found, it will then attempt to load the default options. If that fails too, an error is raised.
#' @param options_name [character, optional] Name of the options to load, excluding the .yaml suffix. Defaults to NULL.
#' @param options_dir [character, optional] Path to the folder in which to look for user options files. Defaults to NULL.
#' @return NULL Loads the options into the options() namespace.
#' @export
load_user_options <- function(options_name = NULL, options_dir = NULL) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  if (!is.null(options_name)) {
    # Search for the user-specified options file first

    if (grepl(".yaml$", options_name)) {
      rlang::abort(glue::glue("Please pass the name of the options to load without the .yaml suffix. Got: {options_name}."))
    }

    file_name <- glue::glue("{options_name}.yaml")
    file_path <- file.path(options_dir, file_name)
    load_user_options_file(path = file_path)
    return(invisible(NULL))
  }

  # Check whether there is a current options configuration. Load it if so.
  current_options_file_path <- file.path(options_dir, CONST$CURRENT_OPTIONS_FILE_NAME)

  if (file.exists(current_options_file_path)) {
    load_user_options_file(path = current_options_file_path)
    current_options_name <- getOption("artma.general.name")
    logger::log_info(glue::glue("Found an existing configuration: '{current_options_name}'. Loading the options from there..."))
    return(invisible(NULL))
  }

  default_options_file_path <- PATHS$FILE_OPTIONS_DEFAULT

  if (file.exists(default_options_file_path)) {
    logger::log_info("Loading default options...")
    load_user_options_file(path = default_options_file_path)
    return(invisible(NULL))
  }

  rlang::abort("Could not load user options.")
}
