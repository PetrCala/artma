#' Validate a user options file against an options template.
#'
#' This function reads a YAML template and an options file, flattens both structures,
#' and then checks that:
#'  - Every option defined in the template is present in the options file.
#'  - The value for each option is of the correct type.
#'  - (Optionally) It warns about extra options in the file that are not defined in the template.
#'
#' For each problem found (missing option or type mismatch), an error message is printed.
#'
#' @param options_file_name *\[character\]* Name of the user options file to validate, including the suffix.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @param should_flag_redundant *\[logical, optional\]* If TRUE, warn the user about any extraneous options (i.e., options not defined in the options template, such as custom options that the user might have added). Defaults to FALSE.
#' @param template_path *\[character, optional\]* Full path to the options template file. Defaults to `NULL`.
#' @param failure_action *\[character\]* Action to take if validation fails. Can be one of: 'abort_verbose', 'abort_quiet', 'return_errors_verbose', 'return_errors_quiet'. Defaults to 'abort_verbose'.
#' @param verbose *\[logical, optional\]* If TRUE, print additional information about the validation process. Defaults to TRUE.
#' `list` Invisibly returns a list of error messages (empty if no errors).
#' @return *\[list\]* The validation errors
#' @export
options.validate <- function(
    options_file_name = NULL,
    options_dir = NULL,
    should_flag_redundant = FALSE,
    template_path = NULL,
    failure_action = "abort_verbose",
    verbose = TRUE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / options / ask[ask_for_existing_options_file_name],
    artma / options / utils[
      get_expected_type,
      nested_to_flat,
      validate_option_value
    ],
    artma / options / template[flatten_template_options],
    artma / libs / validation[validate_value_type, assert, validate, assert_options_template_exists]
  )

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  assert_options_template_exists(template_path)

  validate(is.logical(should_flag_redundant))
  assert(
    failure_action %in% CONST$OPTIONS$VALIDATION_ACTIONS,
    glue::glue("Invalid failure action specified: {failure_action}. Must be one of: {glue::glue_collapse(CONST$OPTIONS$VALIDATION_ACTIONS, sep = ", ")}")
  )

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG
  if (!dir.exists(options_dir)) {
    cli::cli_abort(glue::glue("The following options directory does not exist: {options_dir}"))
  }

  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the user options file you wish to delete: ")
  options_path <- file.path(options_dir, options_file_name)

  assert(file.exists(options_path), glue::glue("Options file '{options_path}' does not exist."))

  if (verbose) {
    cli::cli_inform("Validating the user options file {.file {options_file_name}}...")
  }

  # Load the YAML files
  template <- yaml::read_yaml(template_path)
  options_file <- yaml::read_yaml(options_path)

  template_defs <- flatten_template_options(template) # Flatten the template
  flat_options <- nested_to_flat(options_file) # Flatten the options

  errors <- list()

  # Validate that every expected option is provided and has the correct type.
  for (opt_def in template_defs) {
    opt_name <- opt_def$name
    allow_na <- opt_def$allow_na
    exp_type <- get_expected_type(opt_def)

    if (!(opt_name %in% names(flat_options))) {
      errors[[length(errors) + 1]] <- list(
        type = "missing_option",
        value = NULL,
        opt_def = opt_def,
        message = paste0("Missing option: '", opt_name, "'")
      )
    } else {
      # Option exists, validate its type/value
      value <- flat_options[[opt_name]]
      err_msg <- validate_option_value(value, exp_type, opt_name, allow_na)
      if (!is.null(err_msg)) {
        errors[[length(errors) + 1]] <- list(
          type = "type_mismatch",
          value = value,
          opt_def = opt_def,
          message = err_msg
        )
      }
    }
  }

  # Warn about extraneous options in the file.
  if (should_flag_redundant) {
    for (opt_name in names(flat_options)) {
      if (!any(vapply(template_defs, function(x) identical(x$name, opt_name), logical(1)))) {
        cli::cli_alert_warning(paste0(
          "Extraneous option: '", opt_name,
          "' is not defined in the template."
        ))
      }
    }
  }

  print_validation_results <- function() {
    if (length(errors) > 0) {
      cli::cli_alert_danger("Validation failed.")

      cli::cli_h1("Validation errors found:")
      for (err in errors) {
        cli::cli_alert_danger(err$message)
      }

      cli::cli_h3("Possible Resolutions:")
      cli::cli_ul()
      cli::cli_li("Run {.code artma::options.help(c('opt.name1', 'opt.name2', ...))} to view detailed descriptions of the specified options.")
      cli::cli_li("Run {.code artma::options.modify()} to manually modify the options file.")
      cli::cli_li("Run {.code artma::options.fix()} to automatically fix detected errors where possible.")
      cli::cli_end()
      cli::cat_line()
    } else {
      if (verbose) {
        cli::cli_alert_success("The user options file {.path {options_file_name}} is valid.")
      }
    }
  }

  if (grepl("verbose", failure_action)) print_validation_results()
  if (grepl("abort", failure_action) && length(errors) > 0) {
    cli::cli_abort(glue::glue("Validation failed for file {options_file_name}."))
  }

  invisible(errors)
}

#' @title Copy user options
#' @description Provide a name of a user options file to copy from, and a name of a file to copy to, and copy from the 'from' file to the 'to' file.
#' @param options_file_name_from *\[character, optional\]* Name of the options file to copy from. If not provided, the user will be prompted. Defaults to \code{NULL}.
#' @param options_file_name_to *\[character, optional\]* Name of the options file to copy to. If not provided, the user will be prompted. Defaults to \code{NULL}.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @return `NULL`
#' @export
options.copy <- function(
    options_file_name_from = NULL,
    options_file_name_to = NULL,
    options_dir = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_options_file_name, ask_for_existing_options_file_name],
    artma / libs / validation[assert]
  )

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG
  options_file_name_from <- options_file_name_from %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the name of the user options file you wish to copy from: ")
  options_file_path_from <- file.path(options_dir, options_file_name_from)

  assert(file.exists(options_file_path_from), cli::format_inline("The source options file does not exist under the following path: {.path {options_file_path_from}}"))

  options_file_name_to <- options_file_name_to %||% ask_for_options_file_name(prompt = "Please provide a name for your new options file, including the .yaml suffix: ")
  options_file_path_to <- file.path(options_dir, options_file_name_to)

  if (file.exists(options_file_path_to)) {
    overwrite_permitted <- utils::select.list(
      title = cli::format_inline("An options file name already exists under the path {.path {options_file_path_to}}. Do you wish to overwrite the contents of this file?"),
      choices = c("Yes", "No")
    )
    if (overwrite_permitted != "Yes") {
      cli::cli_abort("Aborting the copying of a user options file.")
    }
  }

  file.copy(options_file_path_from, options_file_path_to, overwrite = TRUE)

  cli::cli_alert_success("The user options file {.path {options_file_name_from}} has been successfully copied over to {.path {options_file_name_to}}.")
}

#' @title Delete user options
#' @description Provide a name of a user options file to delete, and delete that file.
#' @param options_file_name *\[character, optional\]* Name of the options file to delete. If not provided, the user will be prompted. Defaults to `NULL`.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @param skip_confirmation *\[boolean, optional\]* If passed as TRUE, the user will not be prompted for deletion confirmation. Defaults to FALSE.
#' @return `NULL`
#' @export
options.delete <- function(
    options_file_name = NULL,
    options_dir = NULL,
    skip_confirmation = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_existing_options_file_name],
    artma / libs / validation[assert, validate]
  )

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG
  options_file_names <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the user options files you wish to delete: ", multiple = TRUE) # nolint: unused_declared_object_linter.

  if (!skip_confirmation) {
    deletion_confirmed <- utils::select.list(
      title = cli::format_inline("Are you sure you wish to delete {.file {options_file_names}}?"),
      choices = c("Yes, I am sure", "No, I did not mean to")
    )
    if (deletion_confirmed != "Yes, I am sure") {
      cli::cli_abort("Aborting user option file deletion.")
    }
  }

  invisible(lapply(options_file_names, function(file_name) {
    options_file_path <- file.path(options_dir, file_name)

    validate(is.logical(skip_confirmation))
    assert(file.exists(options_file_path), cli::format_inline("The user options file does not exist under the following path: {.file {options_file_path}}"))

    base::file.remove(options_file_path)

    cli::cli_alert_success("The user options file {.file {file_name}} has been deleted.")
  }))
}

#' @title List available user options
#' @description Retrieves the list of the existing options files and returns their names as a character vector. By default, this retrieves the names of the files including the yaml suffix, but can be modified to retrieve options verbose names instead.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @param should_return_verbose_names *\[logical, optional\]* If set to TRUE, the custom names of each of the options files are read and returned instead of file names. Defaults to FALSE.
#' @return *\[vector, character\]* A character vector with the names of the options available.
#' @export
options.list <- function(options_dir = NULL, should_return_verbose_names = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / const[CONST]
  )
  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG

  if (!dir.exists(options_dir)) {
    return(character(0))
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
          cli::cli_inform("Reading the options file {.path {file_name}}")
          options_name <- yaml::read_yaml(file_name)$general$name
        },
        error = function(cond) {
          cli::cli_alert_warning("Failed to read the following options file: {.path {file}}")
        }
      )
    } else {
      file_name
    }
    options_names <- append(options_names, options_name)
  }
  return(options_names)
}

#' @title Load user options
#' @description Load user options by their name and return them as a list.
#' @details In case the options name is not passed, the function will attempt to load the current options configuration. If none is found, it will then attempt to load the default options. If that fails too, an error is raised.
#' @param options_file_name *\[character, optional\]* Name of the options to load, including the .yaml suffix. Defaults to `NULL`.
#' @param options_dir *\[character, optional\]* Path to the folder in which to look for user options files. Defaults to `NULL`.
#' @param create_options_if_null *\[logical, optional\]* If set to TRUE and the options file name is set to NULL, the function will prompt the user to create a new options file. Defaults to TRUE.
#' @param load_with_prefix *\[logical, optional\]* Whether the options should be loaded with the package prefix. Defaults to TRUE.
#' @param should_validate *\[logical, optional\]* Whether the options should be validated after loading. Defaults to TRUE.
#' @param should_set_to_namespace *\[logical, optional\]* Whether the options should be set in the options() namespace. Defaults to TRUE.
#' @param should_return *\[logical, optional\]* Whether the function should return the list of options. Defaults to FALSE.
#' @return *\[list|NULL\]* The loaded options as a list or `NULL`.
#' @export
options.load <- function(
    options_file_name = NULL,
    options_dir = NULL,
    create_options_if_null = TRUE,
    load_with_prefix = TRUE,
    should_validate = TRUE,
    should_set_to_namespace = FALSE, # Be careful when setting this to TRUE - consider using withr::with_options instead
    should_return = TRUE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / options / utils[nested_to_flat, remove_options_with_prefix],
    artma / libs / utils[is_empty],
    artma / libs / validation[validate, assert]
  )

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG

  validate(
    is.character(options_dir),
    is.logical(create_options_if_null),
    is.logical(load_with_prefix),
    is.logical(should_validate),
    is.logical(should_set_to_namespace),
    is.logical(should_return)
  )

  if (is.null(options_file_name)) {
    existing_options_files <- options.list(options_dir = options_dir)

    if (is_empty(existing_options_files)) {
      if (!create_options_if_null) {
        cli::cli_abort("No user options file to load was provided. Exiting...")
      }
      can_proceed <- utils::select.list(
        title = "We have not found any option files to load for you. Would you like to create one now?",
        choices = c("Yes", "No")
      )
      if (can_proceed != "Yes") {
        cli::cli_abort("To load user options, you must create an options file first.")
      }
      options_file_name <- options.create(
        options_file_name = options_file_name,
        options_dir = options_dir
      )
    } else {
      if (!interactive()) {
        cli::cli_abort("No user options file to load was provided. Exiting...")
      }

      action <- utils::select.list(
        title = "You have not specified the options file name to load. Please choose one of the following:",
        choices = c("Create a new options file", "Choose from existing options files")
      )

      if (action == "Create a new options file") {
        options_file_name <- options.create(
          options_file_name = options_file_name,
          options_dir = options_dir
        )
      } else if (action == "Choose from existing options files") {
        options_file_name <- utils::select.list(
          title = "Please choose an options file to load:",
          choices = existing_options_files
        )
        if (is_empty(options_file_name)) {
          cli::cli_abort("No user options file was selected. Aborting...")
        }
      } else {
        cli::cli_abort("No action was chosen for loading user options. Exiting...")
      }
    }
  }

  assert(
    grepl(".yaml$|.yml$", options_file_name),
    glue::glue("Please pass the name of the options to load with the .yaml suffix. Got: {options_file_name}.")
  )

  options_file_path <- file.path(options_dir, options_file_name)
  nested_options <- yaml::read_yaml(options_file_path)

  parent_key <- if (load_with_prefix) CONST$PACKAGE_NAME else NULL
  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = parent_key)

  if (should_validate) {
    options.validate(
      options_file_name = options_file_name,
      options_dir = options_dir,
      failure_action = "abort_verbose",
      verbose = FALSE
    )
  }

  cli::cli_inform("Loading options from the following user options file: {.file {options_file_name}}")

  if (should_set_to_namespace) {
    remove_options_with_prefix(CONST$PACKAGE_NAME) # Remove all existing package options
    options(prefixed_options)
  }

  if (should_return) {
    return(prefixed_options)
  }

  invisible(NULL)
}

#' @title Modify User Options
#' @description Modify an existing user options file with new values.
#'
#' @param options_file_name *\[character, optional\]* Name of the user options file to modify, including the suffix.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @param template_path *\[character, optional\]* Full path to the options template file. Defaults to `NULL`.
#' @param user_input *\[list, optional\]* A named list of user-supplied values for these options. If `NULL` or missing entries exist, the function will prompt the user via `readline()` (for required entries) or use defaults (for optional ones).
#' @param should_validate *\[logical, optional\]* If TRUE, validate the modified options file against the template. Defaults to TRUE.
#' @return `NULL`
#' @export
options.modify <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL,
    user_input = list(),
    should_validate = TRUE) {
  box::use(
    artma / options / ask[
      ask_for_existing_options_file_name,
      ask_for_options_to_modify
    ],
    artma / libs / validation[assert, validate]
  )

  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the name of the user options file you wish to modify: ")

  validate(
    is.list(user_input),
    is.logical(should_validate)
  )

  current_options <- options.load(
    options_file_name = options_file_name,
    options_dir = options_dir,
    create_options_if_null = FALSE,
    load_with_prefix = FALSE,
    should_validate = TRUE,
    should_set_to_namespace = FALSE,
    should_return = TRUE
  )

  if (length(user_input) == 0) {
    if (!interactive()) {
      cli::cli_abort("If you wish to modify a user options file, you must provide a list of options to modify, together with their values.")
    }

    user_input <- ask_for_options_to_modify()
  }

  new_options <- utils::modifyList(current_options, user_input)

  options.create(
    options_file_name = options_file_name,
    options_dir = options_dir,
    template_path = template_path,
    user_input = new_options,
    should_validate = should_validate,
    should_overwrite = TRUE,
    action_name = "modifying"
  )

  invisible(NULL)
}

#' @title Options Open
#' @description Open an options file for editing. Must be run interactively.
#' @param options_file_name *\[character, optional\]* Name of the user options file to modify, including the suffix.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @return `NULL` Opens the file for editing
#' @export
options.open <- function(
    options_file_name = NULL,
    options_dir = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_existing_options_file_name]
  )

  if (!rlang::is_interactive()) {
    cli::cli_alert_warning("Running in non-interactive mode. The user options file cannot be opened.")
    return(invisible(NULL))
  }

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG
  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the name of the user options file you wish to open: ")

  file_path <- file.path(options_dir, options_file_name)
  usethis::edit_file(file_path)

  return(invisible(NULL))
}

#' @title Options Help
#' @description
#' Prints information for each requested option (or all options if `options` is `NULL`).
#'
#' @param options *\[character, optional\]* A single option name (dot-separated) or a
#'   character vector thereof. If `NULL`, prints **all** options from"
#'   the template.
#' @param template_path *\[character, optional\]* Path to the template YAML file.
#'   Defaults to \code{PATHS$FILE_OPTIONS_TEMPLATE}.
#' @return Invisibly returns `NULL`, printing the requested information
#'   to the console.
#' @export
options.help <- function(
    options = NULL,
    template_path = NULL) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / options / template[flatten_template_options],
    artma / libs / validation[assert, assert_options_template_exists, validate]
  )

  if (is.null(options)) {
    cli::cli_alert_warning("Enter option names either as a character vector or as a single name to print their help.\n")
    return(invisible(NULL))
  }
  validate(is.character(options))

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  assert_options_template_exists(template_path)

  template_raw <- yaml::read_yaml(template_path)
  template_defs <- flatten_template_options(template_raw)

  # Build a named lookup table: name -> option definition
  #    Each item typically has `name`, `type`, `default`, `help`, possibly others.
  template_map <- stats::setNames(template_defs, vapply(template_defs, `[[`, character(1), "name"))

  not_found <- setdiff(options, names(template_map))
  if (length(not_found) > 0) {
    msg <- paste(
      "The following requested option(s) are not recognized:",
      paste(not_found, collapse = ", ")
    )
    cli::cli_alert_warning(msg)
    return(invisible(NULL))
  }

  if (length(options) == 0) {
    cli::cli_alert_info("No options to explain.\n")
    return(invisible(NULL))
  }

  cli::cli_h1("Options Help")
  cli::cat_line()

  for (opt_name in options) {
    opt_def <- template_map[[opt_name]]

    # nolint start: unused_declared_object_linter.
    nm <- opt_def$name
    tp <- opt_def$type
    hlp <- opt_def$help %||% "(No help text provided.)"

    if ("default" %in% names(opt_def)) {
      # Accessing a null default value would assign null, so we coerce it to a 'null' string.
      def <- opt_def$default %||% "null"
    } else {
      def <- "This option is required"
    }
    # nolint end: unused_declared_object_linter.

    opt_styles <- CONST$STYLES$OPTIONS # nolint: unused_declared_object_linter.
    cli::cli_text("{.strong Option name:} {opt_styles$NAME(nm)}")
    cli::cli_text("{.strong Type:} {opt_styles$TYPE(tp)}")
    cli::cli_text("{.strong Default:} {opt_styles$DEFAULT(def)}")
    cli::cli_text("{.strong Help:} {.emph {hlp}}")
    cli::cli_rule()
    cli::cat_line()
  }

  invisible(NULL)
}

#' @title Print default user options directory
#' @description Prints the full path to the directory where user options are stored by default
#' @param ... *\[any\]* Additional arguments.
#' @return `NULL` Prints the default directory to console.
#' @export
options.print_default_dir <- function(...) { # nolint: object_name_linter.
  box::use(artma / paths[PATHS])

  cli::cli_h1("User option files default directory:")
  cli::cli_text("{.path {PATHS$DIR_USR_CONFIG}}")
}

#' @title Fix user options file
#' @description Fix a user options file by setting the default values for missing options.
#' @details The function will attempt to load the user options file and validate it. If any errors are found, the function will attempt to fix them by setting the default values for the missing options.
#' @param options_file_name *\[character, optional\]* Name of the options file to fix, including the .yaml suffix. Defaults to `NULL`.
#' @param options_dir *\[character, optional\]* Path to the folder in which to look for user options files. Defaults to `NULL`.
#' @param template_path *\[character, optional\]* Path to the options template file. Defaults to `NULL`.
#' @param force_default_overwrites *\[logical, optional\]* If set to TRUE, the function will overwrite the existing options file with the default values. Defaults to TRUE.
#' @return `NULL` Fixes the user options file.
#' @export
options.fix <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL,
    force_default_overwrites = TRUE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / options / ask[ask_for_existing_options_file_name, ask_for_option_value],
    artma / options / utils[nested_to_flat, parse_options_file_name],
    artma / libs / validation[validate]
  )

  validate(is.logical(force_default_overwrites))


  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the name of the user options file you wish to fix: ")
  options_file_name <- parse_options_file_name(options_file_name)

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG

  expected_path <- file.path(options_dir, options_file_name)
  if (!file.exists(expected_path)) {
    cli::cli_abort(cli::format_inline("The user options file {.file {options_file_name}} does not exist under path {.path {expected_path}}."))
  }

  errors <- options.validate(
    options_file_name = options_file_name,
    options_dir = options_dir,
    failure_action = "return_errors_quiet",
    verbose = FALSE
  )

  if (length(errors) == 0) {
    cli::cli_alert_success("No errors found in the user options file '{.file {options_file_name}}'.")
    return(invisible(NULL))
  }

  cli::cli_h1("Fixing User Options File")
  cli::cli_text("We have detected errror in the user options file: {.file {options_file_name}}.")

  fixed_options <- list()
  proposed_changes <- list()
  has_printed_missing_message <- FALSE

  for (err in errors) {
    opt_def <- err$opt_def
    opt_name <- opt_def$name
    opt_value <- if (is.null(err$value)) "null" else err$value # nolint: unused_declared_object_linter.
    if (is.null(opt_def$default)) {
      if (!has_printed_missing_message) {
        cli::cli_h3("Missing Required Options:")
        cli::cli_ul()
        cli::cli_li("Some required options are missing and have no default values. Please provide the values for these options.")
        cli::cli_li("{.strong Syntax}: {CONST$STYLES$OPTIONS$NAME('<option_name>')}: {CONST$STYLES$OPTIONS$VALUE('<old_value>')} -> {CONST$STYLES$OPTIONS$VALUE('<new_value>')}")
        cli::cli_end()
        cli::cat_line()
        has_printed_missing_message <- TRUE
      }
      # A required option is missing and has no default value - ask the user for input
      fixed_value <- ask_for_option_value(
        option_name = opt_name,
        option_type = opt_def$type,
        allow_na = opt_def$allow_na
      )
    } else if (err$type == "missing_option" || force_default_overwrites) {
      fixed_value <- opt_def$default
    } else if (err$type == "type_mismatch") {
      fixed_value <- opt_def$default # Here, the user could be asked for input as well
    } else {
      cli::cli_abort("Unknown error type encountered while fixing the user options file.")
    }
    fixed_options[[opt_name]] <- fixed_value
    proposed_changes <- append(
      proposed_changes,
      glue::glue("{CONST$STYLES$OPTIONS$NAME(opt_name)}: {CONST$STYLES$OPTIONS$VALUE(opt_value)} -> {CONST$STYLES$OPTIONS$VALUE(fixed_value)}")
    )
  }

  cli::cli_h3("Proposed Changes:")
  cli::cli_ul()
  for (change in proposed_changes) {
    cli::cli_li(change)
  }
  cli::cli_end()

  cli::cat_line()

  if (interactive()) {
    should_proceed <- utils::select.list(
      title = "Do you wish to apply the proposed changes to the user options file?",
      choices = c("Yes", "No")
    )
    if (should_proceed != "Yes") {
      cli::cli_abort("Aborting the fixing of a user options file.")
    }
  } else {
    cli::cli_alert_info("Running in non-interactive mode. The proposed changes will be applied automatically.")
  }

  cli::cli_inform("Fixing the user options file {.path {options_file_name}}...")

  options.create(
    options_file_name = options_file_name,
    options_dir = options_dir,
    template_path = template_path,
    user_input = fixed_options,
    should_validate = TRUE,
    should_overwrite = TRUE,
    action_name = "fixing"
  )

  invisible(NULL)
}

#' @title Create user options
#' @description Create a new user options file from an options template.
#' @param options_file_name *\[character\]* Name of the new user options file, including the suffix.
#' @param options_dir *\[character, optional\]* Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to `NULL`.
#' @param template_path *\[character, optional\]* Full path to the options template file.
#' @param user_input *\[list, optional\]* A named list of user-supplied values for these options. If `NULL` or missing entries exist, the function will prompt the user via `readline()` (for required entries) or use defaults (for optional ones).
#' @param should_validate *\[logical, optional\]* If TRUE, validate the new options file against the template. Defaults to TRUE.
#' @param should_overwrite *\[logical, optional\]* If TRUE, overwrite the file if it already exists. Defaults to FALSE, in which case the user is prompted to confirm the overwrite.
#' @param action_name *\[character, optional\]* A name for the action being performed. This is used for logging purposes. Defaults to "create".
#' `character` Name of the newly created user options file as a character.
#' @return `NULL`
#' @export
options.create <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL,
    user_input = list(),
    should_validate = TRUE,
    should_overwrite = FALSE,
    action_name = "creating") {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_options_file_name],
    artma / options / template[parse_options_from_template],
    artma / options / utils[
      flat_to_nested,
      nested_to_flat,
      parse_options_file_name
    ],
    artma / libs / file_utils[ensure_folder_existence],
    artma / libs / validation[assert, validate]
  )

  validate(is.character(action_name))

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  validate(
    is.character(template_path),
    is.list(user_input)
  )

  options_file_name <- options_file_name %||% ask_for_options_file_name()
  options_file_name <- parse_options_file_name(options_file_name)

  cli::cli_inform("{stringr::str_to_title(action_name)} a user options file: {.path {options_file_name}}")

  options_dir <- options_dir %||% PATHS$DIR_USR_CONFIG
  options_file_path <- file.path(options_dir, options_file_name)

  assert(
    is.character(options_file_path) && length(options_file_path) > 0,
    cli::format_inline("Invalid options file path: {.path {options_file_path}}")
  )

  parsed_options <- if (!file.exists(options_file_path)) {
    parse_options_from_template(
      path         = template_path,
      user_input   = user_input,
      interactive  = TRUE,
      add_prefix   = FALSE
    )
  } else {
    file_exists_msg <- cli::format_inline("An options file {.path {options_file_name}} already exists.")

    if (isTRUE(should_overwrite)) {
      cli::cli_inform("{file_exists_msg} Overwriting this file...")
    } else {
      if (!interactive()) {
        cli::cli_abort("{file_exists_msg} Either allow overwriting or provide a different name.")
      }
      overwrite_permitted <- utils::select.list(
        title = paste(file_exists_msg, "Do you wish to overwrite the contents of this file?"),
        choices = c("Yes", "No")
      )
      if (overwrite_permitted != "Yes") {
        cli::cli_abort("Aborting the overwriting of a user options file.")
      }
    }
    current_options <- nested_to_flat(yaml::read_yaml(options_file_path))
    utils::modifyList(current_options, user_input)
  }

  nested_options <- flat_to_nested(parsed_options)

  ensure_folder_existence(dirname(options_file_path))
  yaml::write_yaml(nested_options, options_file_path)

  if (should_validate) {
    options.validate(
      options_file_name = options_file_name,
      options_dir = options_dir,
      failure_action = "abort_verbose",
      verbose = FALSE # No info messages
    )
  }

  invisible(options_file_name)
}

# Copy an existing folder of options into another place
# options.migrate

# Inspect an existing user options file
# options.inspect
