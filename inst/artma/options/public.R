#' This is a public package method. For more information, see 'options.R::options.create'.
create_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / template[parse_options_from_template],
    artma / options / utils[
      flat_to_nested,
      parse_options_file_name,
      ask_for_options_file_name
    ],
    artma / libs / file_utils[ensure_folder_existence],
    artma / libs / validation[assert]
  )

  options_file_name <- options_file_name %||% ask_for_options_file_name()

  # Ensure valid, constant, and parseable format for the options file names
  options_file_name <- parse_options_file_name(options_file_name)

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_path <- file.path(options_dir, options_file_name)
  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE

  assert(is.character(template_path), glue::glue("'template_path' must be a character. Got '{template_path}'."))

  args <- commandArgs(trailingOnly = TRUE)
  parsed_options <- parse_options_from_template(path = template_path, args = args)

  assert(is.list(parsed_options), "Invalid parsed options - must be a list.")
  assert(
    is.character(options_file_path) && length(options_file_path) > 0,
    glue::glue("Invalid options file path: {options_file_path}")
  )
  if (file.exists(options_file_path)) {
    logger::log_info(glue::glue("An options file already exists under the path {options_file_path}. Overwriting this file..."))
  }

  nested_options <- flat_to_nested(parsed_options)

  logger::log_info(glue::glue("Creating a new user options file: '{options_file_name}'..."))

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
  box::use(
    artma / paths[PATHS],
    artma / options / utils[ask_for_options_file_name, ask_for_existing_options_file_name],
    artma / libs / validation[assert]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_name_from <- options_file_name_from %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the name of the user options file you wish to copy from: ")
  options_file_path_from <- file.path(options_dir, options_file_name_from)

  assert(file.exists(options_file_path_from), glue::glue("The source options file does not exist under the following path: {options_file_path_from}"))

  options_file_name_to <- options_file_name_to %||% ask_for_options_file_name(prompt = "Please provide a name for your new options file, including the .yaml suffix: ")
  options_file_path_to <- file.path(options_dir, options_file_name_to)

  if (file.exists(options_file_path_to)) {
    overwrite_permitted <- utils::select.list(
      title = "An options file name already exists under the path {options_file_path_to}. Do you wish to overwrite the contents of this file?",
      choices = c("Yes", "No")
    )
    if (overwrite_permitted != "Yes") {
      stop("Aborting the copying of a user options file.")
    }
  }

  file.copy(options_file_path_from, options_file_path_to, overwrite = TRUE)

  logger::log_info(glue::glue("The user options file '{options_file_name_from}' has been successfully copied over to '{options_file_name_to}'."))
}

#' This is a public package method. For more information, see 'options.R::options.delete'.
delete_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    skip_confirmation = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / options / utils[ask_for_existing_options_file_name],
    artma / libs / validation[assert, validate]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the user options file you wish to delete: ")
  options_file_path <- file.path(options_dir, options_file_name)

  validate(is.logical(skip_confirmation))
  assert(file.exists(options_file_path), glue::glue("The user options file does not exist under the following path: {options_file_path}"))

  if (!skip_confirmation) {
    deletion_confirmed <- utils::select.list(
      title = glue::glue("Are you sure you wish to delete the file the user options file '{options_file_name}'?"),
      choices = c("Yes, I am sure", "No, I want to keep the file")
    )
    if (deletion_confirmed != "Yes, I am sure") {
      stop("Aborting the deletion of a user options file.")
    }
  }

  base::file.remove(options_file_path)

  logger::log_info(glue::glue("The user options file '{options_file_name}' has been deleted."))
}


#' This is a public package method. For more information, see 'options.R::options.'.
validate_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    should_flag_redundant = FALSE,
    should_fail = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / options / utils[
      ask_for_existing_options_file_name,
      get_expected_type,
      nested_to_flat
    ],
    artma / options / template[flatten_template_options],
    artma / libs / validation[validate_value_type, assert, validate]
  )

  template_path <- PATHS$FILE_OPTIONS_TEMPLATE

  assert(
    file.exists(template_path),
    glue::glue("The options template file does not exist. Try reinstalling the 'artma' package, and if that does not help, please contact the package maintainer.")
  )
  validate(
    is.logical(should_flag_redundant),
    is.logical(should_fail)
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  if (!dir.exists(options_dir)) {
    rlang::abort(glue::glue("The following options directory does not exist: {options_dir}"))
  }

  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the user options file you wish to delete: ")
  options_path <- file.path(options_dir, options_file_name)

  assert(file.exists(options_path), glue::glue("Options file '{options_path}' does not exist."))

  logger::log_info(glue::glue("Validating the user options file '{options_file_name}'..."))

  # Load the YAML files
  template <- yaml::read_yaml(template_path)
  options_file <- yaml::read_yaml(options_path)

  template_defs <- flatten_template_options(template) # Flatten the template
  flat_options <- nested_to_flat(options_file) # Flatten the options


  errors <- character(0)

  # Validate that every expected option is provided and has the correct type.
  for (opt_def in template_defs) {
    dest <- opt_def$dest
    exp_type <- get_expected_type(opt_def)

    if (!(dest %in% names(flat_options))) {
      errors <- c(errors, paste0("Missing option: '", dest, "'"))
    } else {
      value <- flat_options[[dest]]
      if (!validate_value_type(value, exp_type)) {
        errors <- c(
          errors,
          paste0(
            "Incorrect type for option '", dest,
            "': expected '", exp_type, "', got '", class(value)[1], "'"
          )
        )
      }
    }
  }

  # Warn about extraneous options in the file.
  if (should_flag_redundant) {
    for (opt_name in names(flat_options)) {
      if (!any(vapply(template_defs, function(x) identical(x$dest, opt_name), logical(1)))) {
        warning(paste0(
          "Extraneous option: '", opt_name,
          "' is not defined in the template."
        ))
      }
    }
  }

  if (length(errors) > 0) {
    logger::log_error("Validation failed.")

    cli::cli_h1("Validation errors found:")
    for (err in errors) {
      cli::cli_alert_danger(err)
    }
    if (should_fail) {
      rlang::abort(glue::glue("Validation failed for file {options_file_name}."))
    }
  } else {
    logger::log_success(glue::glue("All options are valid."))
  }

  invisible(errors)
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
    artma / options / utils[nested_to_flat, list_user_options_files],
    artma / libs / utils[is_empty],
    artma / libs / validation[validate, assert]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  validate(
    is.character(options_dir),
    is.logical(create_options_if_null),
    is.logical(should_validate),
    is.logical(should_return)
  )

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

  assert(
    grepl(".yaml$|.yml$", options_file_name),
    glue::glue("Please pass the name of the options to load with the .yaml suffix. Got: {options_file_name}.")
  )

  options_file_path <- file.path(options_dir, options_file_name)
  nested_options <- yaml::read_yaml(options_file_path)

  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = CONST$PACKAGE_NAME)

  if (should_validate) {
    validate_user_options_file(
      options_file_name = options_file_name,
      options_dir = options_dir,
      should_fail = TRUE
    )
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
  delete_user_options_file,
  create_user_options_file,
  load_user_options,
  validate_user_options_file
)
