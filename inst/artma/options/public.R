#' This is a public package method. For more information, see 'options.R::options.create'.
create_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL,
    user_input = list(),
    should_validate = TRUE,
    should_overwrite = FALSE,
    action_name = "created") {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_options_file_name],
    artma / options / template[parse_options_from_template],
    artma / options / utils[
      flat_to_nested,
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

  logger::log_info(cli::format_inline("A user options file is being {action_name}: {.path {options_file_name}}..."))

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_path <- file.path(options_dir, options_file_name)

  assert(
    is.character(options_file_path) && length(options_file_path) > 0,
    cli::format_inline("Invalid options file path: {.path {options_file_path}}")
  )

  if (file.exists(options_file_path)) {
    file_exists_msg <- cli::format_inline("An options file {.path {options_file_name}} already exists.")

    if (isTRUE(should_overwrite)) {
      logger::log_info(paste(file_exists_msg, "Overwriting this file..."))
    } else {
      if (!interactive()) {
        rlang::abort(paste(file_exists_msg, "Either allow overwriting or provide a different name."))
      }
      overwrite_permitted <- utils::select.list(
        title = paste(file_exists_msg, "Do you wish to overwrite the contents of this file?"),
        choices = c("Yes", "No")
      )
      if (overwrite_permitted != "Yes") {
        stop("Aborting the overwriting of a user options file.")
      }
    }
  }

  parsed_options <- parse_options_from_template(
    path         = template_path,
    user_input   = user_input,
    interactive  = TRUE,
    add_prefix   = FALSE
  )

  nested_options <- flat_to_nested(parsed_options)

  ensure_folder_existence(dirname(options_file_path))
  yaml::write_yaml(nested_options, options_file_path)

  logger::log_info(cli::format_inline("User options file {action_name}: {.path {options_file_name}}"))

  if (should_validate) {
    validate_user_options_file(
      options_file_name = options_file_name,
      options_dir = options_dir,
      failure_action = "abort_verbose",
      verbose = FALSE # No info messages
    )
  }

  invisible(options_file_name)
}

#' This is a public package method. For more information, see 'options.R::options.copy'.
copy_user_options_file <- function(
    options_file_name_from = NULL,
    options_file_name_to = NULL,
    options_dir = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_options_file_name, ask_for_existing_options_file_name],
    artma / libs / validation[assert]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
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
      stop("Aborting the copying of a user options file.")
    }
  }

  file.copy(options_file_path_from, options_file_path_to, overwrite = TRUE)

  logger::log_info(cli::format_inline("The user options file {.path {options_file_name_from}} has been successfully copied over to {.path {options_file_name_to}}."))
}

#' This is a public package method. For more information, see 'options.R::options.delete'.
delete_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    skip_confirmation = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_existing_options_file_name],
    artma / libs / validation[assert, validate]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  options_file_names <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the user options files you wish to delete: ", multiple = TRUE) # nolint: unused_declared_object_linter.

  if (!skip_confirmation) {
    deletion_confirmed <- utils::select.list(
      title = cli::format_inline("Are you sure you wish to delete {.file {options_file_names}}?"),
      choices = c("Yes, I am sure", "No, I did not mean to")
    )
    if (deletion_confirmed != "Yes, I am sure") {
      stop("Aborting user option file deletion.")
    }
  }

  invisible(lapply(options_file_names, function(file_name) {
    options_file_path <- file.path(options_dir, file_name)

    validate(is.logical(skip_confirmation))
    assert(file.exists(options_file_path), cli::format_inline("The user options file does not exist under the following path: {.file {options_file_path}}"))

    base::file.remove(options_file_path)

    logger::log_info(cli::format_inline("The user options file {.file {file_name}} has been deleted."))
  }))
}


#' This is a public package method. For more information, see 'options.R::options.'.
validate_user_options_file <- function(
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
    failure_action %in% CONST$OPTIONS_VALIDATION_ACTIONS,
    glue::glue("Invalid failure action specified: {failure_action}. Must be one of: {glue::glue_collapse(CONST$OPTIONS_VALIDATION_ACTIONS, sep = ", ")}")
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS
  if (!dir.exists(options_dir)) {
    rlang::abort(glue::glue("The following options directory does not exist: {options_dir}"))
  }

  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the user options file you wish to delete: ")
  options_path <- file.path(options_dir, options_file_name)

  assert(file.exists(options_path), glue::glue("Options file '{options_path}' does not exist."))

  if (verbose) {
    logger::log_debug(glue::glue("Validating the user options file '{options_file_name}'..."))
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
      logger::log_error("Validation failed.")

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
      cat("\n")
    } else {
      if (verbose) {
        cli::cli_alert_success("The user options file {.path {options_file_name}} is valid.")
      }
    }
  }

  if (grepl("verbose", failure_action)) print_validation_results()
  if (grepl("abort", failure_action) && length(errors) > 0) {
    rlang::abort(glue::glue("Validation failed for file {options_file_name}."))
  }

  invisible(errors)
}


#' @title Load user options
#' @description Load user options by their name.
#' @details In case the options name is not passed, the function will attempt to load the current options configuration. If none is found, it will then attempt to load the default options. If that fails too, an error is raised.
#' @param options_file_name [character, optional] Name of the options to load, including the .yaml suffix. Defaults to NULL.
#' @param options_dir [character, optional] Path to the folder in which to look for user options files. Defaults to NULL.
#' @param create_options_if_null [logical, optional] If set to TRUE and the options file name is set to NULL, the function will prompt the user to create a new options file. Defaults to TRUE.
#' @param load_with_prefix [logical, optional] Whether the options should be loaded with the package prefix. Defaults to TRUE.
#' @param should_validate [logical, optional] Whether the options should be validated after loading. Defaults to TRUE.
#' @param should_set_to_namespace [logical, optional] Whether the options should be set in the options() namespace. Defaults to TRUE.
#' @param should_return [logical, optional] Whether the function should return the list of options. Defaults to FALSE.
#' @return NULL Loads the options into the options() namespace.
load_user_options <- function(
    options_file_name = NULL,
    options_dir = NULL,
    create_options_if_null = TRUE,
    load_with_prefix = TRUE,
    should_validate = TRUE,
    should_set_to_namespace = TRUE,
    should_return = FALSE) {
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / options / utils[nested_to_flat, list_user_options_files, remove_options_with_prefix],
    artma / libs / utils[is_empty],
    artma / libs / validation[validate, assert]
  )

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  validate(
    is.character(options_dir),
    is.logical(create_options_if_null),
    is.logical(load_with_prefix),
    is.logical(should_validate),
    is.logical(should_set_to_namespace),
    is.logical(should_return)
  )

  if (is.null(options_file_name)) {
    existing_options_files <- list_user_options_files(options_dir = options_dir)

    if (is_empty(existing_options_files)) {
      if (!create_options_if_null) {
        rlang::abort("No user options file to load was provided. Exiting...")
      }
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

  parent_key <- if (load_with_prefix) CONST$PACKAGE_NAME else NULL
  prefixed_options <- nested_to_flat(nested = nested_options, parent_key = parent_key)

  if (should_validate) {
    validate_user_options_file(
      options_file_name = options_file_name,
      options_dir = options_dir,
      failure_action = "abort_verbose",
      verbose = FALSE
    )
  }

  logger::log_debug(glue::glue("Loading options from the following user options file: '{options_file_name}'"))

  if (should_set_to_namespace) {
    remove_options_with_prefix(CONST$PACKAGE_NAME)
    options(prefixed_options)
  }

  if (should_return) {
    return(prefixed_options)
  }

  invisible(NULL)
}

#' This is a public package method. For more information, see 'options.R::options.modify'.
modify_user_options_file <- function(
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

  current_options <- load_user_options(
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
      rlang::abort("If you wish to modify a user options file, you must provide a list of options to modify, together with their values.")
    }

    user_input <- ask_for_options_to_modify()
  }

  new_options <- utils::modifyList(current_options, user_input)

  create_user_options_file(
    options_file_name = options_file_name,
    options_dir = options_dir,
    template_path = template_path,
    user_input = new_options,
    should_validate = should_validate,
    should_overwrite = TRUE,
    action_name = "modified"
  )

  invisible(NULL)
}

#' This is a public package method. For more information, see 'options.R::options.create'.
options_help <- function(
    options = NULL,
    template_path = NULL) {
  box::use(
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
  cat("\n")

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

    cli::cli_text("{.strong Option name:} {cli::col_magenta(nm)}")
    cli::cli_text("{.strong Type:} {cli::col_cyan(tp)}")
    cli::cli_text("{.strong Default:} {cli::col_yellow(def)}")
    cli::cli_text("{.strong Help:} {.emph {hlp}}")
    cli::cli_rule()
    cat("\n")
  }

  invisible(NULL)
}

#' @title Fix user options file
#' @description Fix a user options file by setting the default values for missing options.
#' @details The function will attempt to load the user options file and validate it. If any errors are found, the function will attempt to fix them by setting the default values for the missing options.
#' @param options_file_name [character, optional] Name of the options file to fix, including the .yaml suffix. Defaults to NULL.
#' @param options_dir [character, optional] Path to the folder in which to look for user options files. Defaults to NULL.
#' @param template_path [character, optional] Path to the options template file. Defaults to NULL.
#' @param force_default_overwrites [logical, optional] If set to TRUE, the function will overwrite the existing options file with the default values. Defaults to TRUE.
#' @return NULL Fixes the user options file.
fix_user_options_file <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL,
    force_default_overwrites = TRUE) {
  box::use(
    artma / paths[PATHS],
    artma / options / ask[ask_for_existing_options_file_name, ask_for_option_value],
    artma / options / utils[nested_to_flat, parse_options_file_name],
    artma / libs / validation[validate]
  )

  validate(is.logical(force_default_overwrites))


  options_file_name <- options_file_name %||% ask_for_existing_options_file_name(options_dir = options_dir, prompt = "Please select the name of the user options file you wish to fix: ")
  options_file_name <- parse_options_file_name(options_file_name)

  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  expected_path <- file.path(options_dir, options_file_name)
  if (!file.exists(expected_path)) {
    rlang::abort(glue::glue("The user options file '{options_file_name}' does not exist under path '{expected_path}'."))
  }

  errors <- validate_user_options_file(
    options_file_name = options_file_name,
    options_dir = options_dir,
    failure_action = "return_errors_quiet",
    verbose = FALSE
  )

  if (length(errors) == 0) {
    cli::cli_alert_success("No errors found in the user options file '{options_file_name}'.")
    return(invisible(NULL))
  }

  cli::cli_h1("Fixing User Options File")
  cli::cli_ul()
  cli::cli_li("Below are the proposed changes to the user options file. Please review them before proceeding.")
  cli::cli_li("{.strong Syntax}: {cli::col_magenta('<option_name>')}: {cli::col_green('<old_value>')} -> {cli::col_green('<new_value>')}")
  cat("\n")

  fixed_options <- list()
  proposed_changes <- list()

  for (err in errors) {
    opt_def <- err$opt_def
    opt_name <- opt_def$name
    opt_value <- if (is.null(err$value)) "null" else err$value # nolint: unused_declared_object_linter.
    if (is.null(opt_def$default)) {
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
      rlang::abort("Unknown error type encountered while fixing the user options file.")
    }
    fixed_options[[opt_name]] <- fixed_value
    proposed_changes <- append(
      proposed_changes,
      glue::glue("{cli::col_magenta(opt_name)}: {cli::col_green(opt_value)} -> {cli::col_green(fixed_value)}")
    )
  }

  cli::cli_h3("Proposed Changes:")
  cli::cli_ul()
  for (change in proposed_changes) {
    cli::cli_li(change)
  }

  cat("\n")

  if (interactive()) {
    should_proceed <- utils::select.list(
      title = "Do you wish to apply the proposed changes to the user options file?",
      choices = c("Yes", "No")
    )
    if (should_proceed != "Yes") {
      stop("Aborting the fixing of a user options file.")
    }
  } else {
    logger::log_warn("Running in non-interactive mode. The proposed changes will be applied automatically.")
  }

  logger::log_info(cli::format_inline("Fixing the user options file {.path {options_file_name}}..."))

  create_user_options_file(
    options_file_name = options_file_name,
    options_dir = options_dir,
    template_path = template_path,
    user_input = fixed_options,
    should_validate = TRUE,
    should_overwrite = TRUE,
    action_name = "fixed"
  )

  invisible(NULL)
}

box::export(
  copy_user_options_file,
  delete_user_options_file,
  create_user_options_file,
  fix_user_options_file,
  load_user_options,
  modify_user_options_file,
  options_help,
  validate_user_options_file
)
