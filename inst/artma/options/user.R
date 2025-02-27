#' This is a public package method. For more information, see 'options.R::options.list'.
list_user_options_files <- function(options_dir = NULL, should_return_verbose_names = FALSE) {
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


#' This is a public package method. For more information, see 'options.R::options.create'.
create_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / template[parse_options_from_template],
    artma / options / utils[flat_to_nested],
    artma / libs / file_utils[ensure_folder_existence]
  )

  if (is.null(options_file_name)) {
    options_file_name <- readline(prompt = "Please provide the name for your options file, including the .yaml suffix: ")
  }

  if (!grepl(".yaml$|.yml$", options_file_name)) {
    rlang::abort(glue::glue("Please pass the options file name with the .yaml suffix."))
  }

  if (is.null(options_dir)) options_dir <- PATHS$DIR_USER_OPTIONS
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

  logger::log_debug(glue::glue("User options file created: '{options_file_name}'"))

  return(options_file_name)
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

    if (!is_empty(existing_options_files)) {
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
    print("Validating the options...")
  }

  logger::log_info(glue::glue("Applying the following options: '{options_file_name}'"))

  options(prefixed_options)

  if (should_return) {
    return(prefixed_options)
  }

  return(invisible(NULL))
}


box::export(
  create_user_options_file,
  list_user_options_files,
  load_user_options
)
