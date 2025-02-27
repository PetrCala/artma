#' This is a public package method. For more information, see 'options.R'.
options.list <- function(options_dir = NULL, should_read_verbose_names = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / const[CONST]
  )

  if (is.null(options_dir)) options_dir <- PATHS$DIR_USER_OPTIONS

  if (!dir.exists(options_dir)) {
    rlang::abort(glue::glue("The following options directory does not exist: {options_dir}"))
  }

  options_files <- list.files(
    path = options_dir,
    pattern = CONST$REGEX$OPTIONS_FILE_SUFFIX,
    # If we are not going to read the file, full names are unnecessary
    full.names = should_read_verbose_names
  )

  options_names <- vector(mode = "character")
  for (file_name in options_files) {
    options_name <- if (should_read_verbose_names) {
      tryCatch(
        {
          logger::log_debug(glue::glue("Reading the options file '{file_name}'"))
          options_name <- yaml::read_yaml(file_name)$general$name
        },
        error = function(cond) {
          logger::log_warn(glue::glue("Failed to read the following options file: {file}"))
        }
      )
    } else {
      file_name
    }
    options_names <- append(options_names, options_name)
  }
  return(options_names)
}


#' @title Create user options file
#' @param options_file_name [character] Name of the options file. Is used only for logging purposes.
#' @param parsed_options [list] A list of parsed options from the template file
#' @param path [character] A path to create the options file.
create_user_options_file <- function(options_file_name, parsed_options, path) {
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

  logger::log_info(glue::glue("Creating a new user options file: '{options_file_name}'"))

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
    artma / options / utils[nested_to_flat]
  )

  nested_options <- yaml::read_yaml(path)

  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = CONST$PACKAGE_NAME)

  # Here, add option file validation functions TODO

  options_file_name <- prefixed_options[["artma.general.name"]] %||% ""
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
#' @param options_file_name [character, optional] Name of the options to load, including the .yaml suffix. Defaults to NULL.
#' @param options_dir [character, optional] Path to the folder in which to look for user options files. Defaults to NULL.
#' @param create_options_if_null [logical, optional] If set to TRUE and the options file name is set to NULL, the function will prompt the user to create a new options file. Defaults to TRUE.
#' @return NULL Loads the options into the options() namespace.
load_user_options <- function(options_file_name = NULL, options_dir = NULL, create_options_if_null = TRUE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS


  if (is.null(options_file_name)) {
    if (!create_options_if_null) {
      rlang::abort("No user options file to load was provided. Exiting...")
    }

    existing_option_files <- options.list()

    if (!!existing_options_files) {
      action <- utils::select.list(
        choices = c("Create a new options file", "Choose from existing options files"),
        title = "You have not specified the options file name to load. Please choose one of the following:"
      )
      if (action == "Choose from existing options files") {
        options_file_name <- utils::select.list(
          choices = existing_option_files,
          title = "Please choose an options file to load:"
        )
      } else if (action == "Create a new options file") {
        print("TODO")
      } else {
        print("Raise an error")
      }
    }
  }

  if (!grepl(".yaml$|.yml$", options_file_name)) {
    rlang::abort(glue::glue("Please pass the name of the options to load with the .yaml suffix. Got: {options_file_name}."))
  }

  file_path <- file.path(options_dir, options_file_name)
  load_user_options_file(path = file_path)

  return(invisible(NULL))
}


box::export(
  create_user_options_file,
  load_user_options,
  options.list
)
