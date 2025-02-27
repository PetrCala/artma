#' This is a public package method. For more information, see 'options.R::options.list'.
list_user_options_files <- function(options_dir = NULL, should_return_verbose_names = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / const[CONST]
  )
  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  if (!dir.exists(options_dir)) {
    rlang::abort(glue::glue("The following options directory does not exist: {options_dir}"))
  }

  options_files <- list.files(
    path = options_dir,
    pattern = CONST$REGEX$OPTIONS_FILE_SUFFIX,
    # If we are not going to read the file, full names are unnecessary
    full.names = should_return_verbose_names
  )

  options_names <- vector(mode = "character")
  for (file_name in options_files) {
    options_name <- if (should_return_verbose_names) {
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


#' @title Ask for options file name
#' @description Ask the user to input a name of an options file. Clean the user's input and return it as a string.
#' @param should_clean [logical, optional] Whether to clean the input string. Defaults to TRUE
#' @param prompt [character, optional] The prompt to use. Defaults to a generic prompt.
#' @return [character] The options file name.
#' @keywords internal
ask_for_options_file_name <- function(should_clean = TRUE, prompt = NULL) {
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(artma / options / utils[flat_to_nested, parse_options_file_name])

  prompt <- prompt %||% "Please provide the name for your options file, including the .yaml suffix: "
  options_file_name <- readline(prompt = prompt)

  if (should_clean) {
    options_file_name <- parse_options_file_name(options_file_name)
  }

  return(options_file_name)
}


#' @title Ask for an existing options file name
#' @description Prompt the user to select from an existing list of user options files. Return the name of the selected file as a character.
#' @param options_dir [character, optional] Name of the directory to list the files from. Defaults to NULL.
#' @param prompt [character, optional] The prompt to use when asking for the user options file name. Defaults to NULL.
#' @return [character] Name of the selected file.
#' @keywords internal
ask_for_existing_options_file_name <- function(options_dir = NULL, prompt = NULL) { # nolint: object_length_linter.
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(artma / libs / utils[is_empty])

  prompt <- prompt %||% "Please select the user options file name you would like to use."

  user_options_file_names <- list_user_options_files(options_dir = options_dir)
  selected_file_name <- utils::select.list(
    title = prompt,
    choices = user_options_file_names
  )
  if (is_empty(selected_file_name)) {
    stop("No user options file was selected. Aborting...")
  }
  return(selected_file_name)
}


#' This is a public package method. For more information, see 'options.R::options.create'.
create_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / template[parse_options_from_template],
    artma / options / utils[flat_to_nested, parse_options_file_name],
    artma / libs / file_utils[ensure_folder_existence]
  )

  options_file_name <- options_file_name %||% ask_for_options_file_name()

  # Ensure valid, constant, and parseable format for the options file names
  options_file_name <- parse_options_file_name(options_file_name)

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_path <- file.path(options_dir, options_file_name)
  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE

  args <- commandArgs(trailingOnly = TRUE)
  parsed_options <- parse_options_from_template(path = template_path, args = args)

  if (!is.list(parsed_options)) {
    rlang::abort("Invalid parsed options - must be a list.")
  }
  if (!is.character(options_file_path) || length(options_file_path) <= 0) {
    rlang::abort(glue::glue("Invalid options file path: {options_file_path}"))
  }
  if (file.exists(options_file_path)) {
    logger::log_info(glue::glue("An options file already exists under the path {options_file_path}. Overwriting this file..."))
  }

  nested_options <- flat_to_nested(parsed_options)

  logger::log_info(glue::glue("Creating a new user options file: '{options_file_name}'"))

  ensure_folder_existence(dirname(options_file_path))
  yaml::write_yaml(
    nested_options,
    options_file_path
  )

  logger::log_info(glue::glue("User options file created: '{options_file_name}'"))

  return(options_file_name)
}

#' This is a public package method. For more information, see 'options.R::options.copy'.
copy_user_options_file <- function(
    options_file_name_from = NULL,
    options_file_name_to = NULL,
    options_dir = NULL) {
  box::use(artma / paths[PATHS])

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_name_from <- options_file_name_from %||% ask_for_existing_options_file_name(prompt = "Please select the name of the user options file you wish to copy from: ")
  options_file_path_from <- file.path(options_dir, options_file_name_from)

  if (!file.exists(options_file_path_from)) {
    rlang::abort(glue::glue("The source options file does not exist under the following path: {options_file_path_from}"))
  }

  options_file_name_to <- options_file_name_to %||% ask_for_options_file_name(prompt = "Please provide a name for your new options file, including the .yaml suffix: ")
  options_file_path_to <- file.path(options_dir, options_file_name_to)

  if (file.exists(options_file_path_to)) {
    overwrite_permitted <- utils::select.list(
      title = "An options file name already exists under the path {}. Do you wish to overwrite the contents of this file?",
      choices = c("Yes", "No")
    )
    if (overwrite_permitted != "Yes") {
      stop("Aborting the copying of a user options file.")
    }
  }

  file.copy(options_file_path_from, options_file_path_to, overwrite = TRUE)

  logger::log_info(glue::glue("The user options file '{options_file_name_from}' has been successfully copied over to '{options_file_name_to}'."))
}

#' @title Load user options
#' @description Load user options by their name.
#' @details In case the options name is not passed, the function will attempt to load the current options configuration. If none is found, it will then attempt to load the default options. If that fails too, an error is raised.
#' @param options_file_name [character, optional] Name of the options to load, including the .yaml suffix. Defaults to NULL.
#' @param options_dir [character, optional] Path to the folder in which to look for user options files. Defaults to NULL.
#' @param create_options_if_null [logical, optional] If set to TRUE and the options file name is set to NULL, the function will prompt the user to create a new options file. Defaults to TRUE.
#' @param should_validate [logical, optional] Whether the options should be validated after loading. Defaults to TRUE.
#' @param should_return [logical, optional] Whether the function should return the list of options. Defaults to FALSE.
#' @return NULL Loads the options into the options() namespace.
load_user_options <- function(
    options_file_name = NULL,
    options_dir = NULL,
    create_options_if_null = TRUE,
    should_validate = TRUE,
    should_return = FALSE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / options / utils[nested_to_flat],
    artma / libs / utils[is_empty]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  if (is.null(options_file_name)) {
    if (!create_options_if_null) {
      rlang::abort("No user options file to load was provided. Exiting...")
    }

    existing_options_files <- list_user_options_files(options_dir = options_dir)

    if (is_empty(existing_options_files)) {
      can_proceed <- utils::select.list(
        title = "We have not found any option files to load for you. Would you like to create one now?",
        choices = c("Yes", "No")
      )
      if (can_proceed != "Yes") {
        stop("To load user options, you must create an options file first.")
      }
      options_file_name <- create_user_options_file(
        options_file_name = options_file_name,
        options_dir = options_dir
      )
    } else {
      action <- utils::select.list(
        title = "You have not specified the options file name to load. Please choose one of the following:",
        choices = c("Create a new options file", "Choose from existing options files")
      )

      if (action == "Create a new options file") {
        options_file_name <- create_user_options_file(
          options_file_name = options_file_name,
          options_dir = options_dir
        )
      } else if (action == "Choose from existing options files") {
        options_file_name <- utils::select.list(
          title = "Please choose an options file to load:",
          choices = existing_options_files
        )
        if (is_empty(options_file_name)) {
          stop("No user options file was selected. Aborting...")
        }
      } else {
        stop("No action was chosen for loading user options. Exiting...")
      }
    }
  }

  if (!grepl(".yaml$|.yml$", options_file_name)) {
    rlang::abort(glue::glue("Please pass the name of the options to load with the .yaml suffix. Got: {options_file_name}."))
  }

  options_file_path <- file.path(options_dir, options_file_name)
  nested_options <- yaml::read_yaml(options_file_path)

  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = CONST$PACKAGE_NAME)

  # Here, add option file validation functions TODO
  if (should_validate) {
    logger::log_info(glue::glue("Validating the following user options file: {options_file_name}"))
  }

  logger::log_info(glue::glue("Loading options from the following user options file: '{options_file_name}'"))

  options(prefixed_options)

  if (should_return) {
    return(prefixed_options)
  }

  return(invisible(NULL))
}


box::export(
  copy_user_options_file,
  create_user_options_file,
  list_user_options_files,
  load_user_options
)
