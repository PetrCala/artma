#' @title Read a template from disk
#' @description Read a template YAML file and remove the temp block. This is the
#'   uncached primitive; callers should go through [parse_template()] so the
#'   parse is shared within a load.
#' @param template_path *\[character\]* Path to the template YAML file.
#' @param mtime *\[numeric\]* Modification time of the template, used only as a
#'   cache key by the memoised wrapper.
#' @keywords internal
.read_template_from_disk <- function(template_path, mtime) { # nolint: object_name_linter.
  template <- yaml::read_yaml(template_path)
  template[["temp"]] <- NULL
  template
}

# Memoised on (path, mtime) so every template consumer within a single load
# shares one disk read. The mtime key invalidates the cache when the file
# changes on disk.
.parse_template_memoised <- memoise::memoise(.read_template_from_disk) # nolint: object_name_linter.

#' @title Parse an options template
#' @description Read and cache a template YAML file, keyed on its path and
#'   modification time. All template consumers share this parse, so a template
#'   is read from disk at most once per load.
#' @param template_path *\[character\]* Path to the template YAML file.
#' @return A list of template options with the temp block removed.
parse_template <- function(template_path) {
  mtime <- as.numeric(file.mtime(template_path))
  .parse_template_memoised(template_path, mtime)
}

#' @title Read template
#' @description Read a template YAML file and remove the temp block.
#' @param template_path *\[character\]* Path to the template YAML file.
#' @return A list of template options.
read_template <- function(template_path) {
  parse_template(template_path)
}

#' @title Check if a node is an option definition
#' @description Check if a node is an option definition.
#' @param e [list] A list of template options.
#' @return A logical value.
#' @keywords internal
is_option_def <- function(e) {
  is.list(e) &&
    all(c("type", "help") %in% names(e))
}

#' @title Flatten nested template options
#' @description Recursively flatten nested template options into a single list of
#' option definitions with flattened destination names (e.g., x.y.z).
#' @param x [list] A list of nested template options.
#' @param parent *\[character\]* The parent path to the current list of options.
#' `list` A list of flattened option definitions.
#' @export
flatten_template_options <- function(x, parent = NULL) {
  flattened <- list()

  for (nm in names(x)) {
    path <- if (is.null(parent)) nm else paste(parent, nm, sep = ".")
    node <- x[[nm]]

    # --> A leaf?  (= list that has a 'type' field)
    if (is_option_def(node)) {
      node$name <- path # <-- add the synthetic name
      flattened[[length(flattened) + 1L]] <- node
      next
    }

    # Otherwise keep descending
    if (is.list(node)) {
      flattened <- c(flattened, flatten_template_options(node, path))
    }
  }

  flattened
}

#' @title Collect leaf paths from template
#' @description Returns a character vector of fully-qualified option names
#' @param template_path *\[character\]* Path to the template YAML file
#' @return A character vector of fully-qualified option names
collect_leaf_paths <- function(template_path) {
  template <- read_template(template_path)
  defs <- flatten_template_options(template)
  vapply(defs, `[[`, character(1), "name")
}

#' @title Get template defaults
#' @description Returns a named list of option defaults from a template, with an optional name prefix.
#' @param template_path *\[character\]* Path to the template YAML file.
#' @param prefix *\[character, optional\]* Optional prefix to prepend to each option name.
#' @return A named list with default values keyed by option name.
get_template_defaults <- function(template_path, prefix = NULL) {
  template <- read_template(template_path)
  defs <- flatten_template_options(template)
  defaults <- list()

  for (opt_def in defs) {
    key <- if (is.null(prefix)) {
      opt_def$name
    } else {
      paste0(prefix, ".", opt_def$name)
    }

    if (!is.null(opt_def$default)) {
      defaults[[key]] <- opt_def$default
    } else if (isTRUE(opt_def$allow_na)) {
      defaults[[key]] <- NA
    }
  }

  defaults
}

#' @title Flatten user options
#' @description Flattens a nested user-supplied YAML object *but* stops at template leaves
#' @param user_options *\[list\]* A nested list returned by yaml::read_yaml()
#' @param leaf_set *\[character\]* A character vector with the leaf paths
#' @param parent *\[character\]* The current parent path
flatten_user_options <- function(user_options, leaf_set, parent = NULL) {
  flat <- list()

  for (nm in names(user_options)) {
    path <- if (is.null(parent)) nm else paste(parent, nm, sep = ".")

    # --> If we've reached a declared template leaf, take the whole value as-is
    if (path %in% leaf_set || !is.list(user_options[[nm]])) {
      flat[[path]] <- user_options[[nm]]
      next
    }

    # Otherwise keep descending
    flat <- c(flat, flatten_user_options(
      user_options = user_options[[nm]],
      leaf_set = leaf_set,
      parent = path
    ))
  }

  flat
}

#' @title Get option definitions
#' @description Get option definitions from a template file.
#' @param template_path *\[character, optional\]* Path to the template YAML file. Defaults to `NULL`.
#' @param opt_path *\[character, optional\]* The name of the option group to get, separated by dots. Defaults to `NULL`.
#' @return A list of option definitions.
get_option_defs <- function(template_path = NULL, opt_path = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / libs / core / validation[assert_options_template_exists, validate_opt_path]
  )

  validate_opt_path(opt_path)

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  assert_options_template_exists(template_path)

  raw_template_options <- read_template(template_path)
  options_def <- flatten_template_options(raw_template_options)

  if (is.null(opt_path)) {
    return(options_def)
  }

  options_def[startsWith(vapply(options_def, `[[`, character(1), "name"), opt_path)]
}


#' @title Prompt user for a required value with no default
#' @description Prompt the user for a value, displaying the option name, type, and help.
#' @param opt [list] Option definition.
#' @keywords internal
prompt_user_for_option_value <- function(opt) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[assert],
    artma / options / utils[print_options_help_text]
  )

  assert(interactive(), "Running in a non-interactive mode. Cannot prompt for required option.")

  cli::cli_h1("Provide Option Value")
  cli::cli_text("{.strong Option name}: {CONST$STYLES$OPTIONS$NAME(opt$name)}")
  cli::cli_text("{.strong Type}: {CONST$STYLES$OPTIONS$TYPE(opt$type)}")
  if (!is.null(opt$default)) {
    cli::cli_text("{.strong Default}: {CONST$STYLES$OPTIONS$DEFAULT(opt$default)}")
  }

  if (!is.null(opt$help) && !isTRUE(opt$suppress_help_in_prompt)) {
    print_options_help_text(opt$help)
  }

  prompt_type <- if (is.null(opt$prompt)) CONST$OPTIONS$DEFAULT_PROMPT_TYPE else opt$prompt

  if (prompt_type == CONST$OPTIONS$PROMPT_TYPES$FUNCTION) {
    if (is.null(opt$prompt_function)) {
      cli::cli_abort(cli::format_inline("Prompt function not provided for option {.strong {opt$name}}."))
    }
    box_import_str <- sprintf("box::use(prompts = artma / options / prompts[%s])", opt$prompt_function)
    tryCatch(
      {
        eval(parse(text = box_import_str))
        prompt_function <- get(opt$prompt_function, prompts)
      },
      error = function(e) {
        cli::cli_abort(cli::format_inline("Prompt function {.strong {opt$prompt_function}} not found."))
      }
    )
    return(prompt_function(opt = opt))
  }

  hints <- switch(prompt_type,
    "file" = cli::format_inline("type in {.emph {'choose'}} or press {.code <Enter>} to select a file interactively"),
    "directory" = cli::format_inline("type in {.emph {'choose'}} or press {.code <Enter>} to select a directory interactively"),
    "readline" = character(0),
    cli::cli_abort(cli::format_inline("Invalid prompt type {.emph {prompt_type}}."))
  )
  if (!is.null(opt$prompt_hint)) {
    hints <- c(hints, cli::format_inline(opt$prompt_hint))
  }
  if (!is.null(opt$default)) {
    hints <- c(hints, cli::format_inline("press {.code <Enter>} to accept default: {.strong {opt$default}}"))
  }

  hint_msg <- if (length(hints)) paste0(" (or ", paste(hints, collapse = ", "), ")") else ""
  input_val <- readline(prompt = cli::format_inline("Enter a value for {.strong {opt$name}}{hint_msg}: "))

  if (input_val == "choose" || is.na(input_val) || (!nzchar(input_val) && prompt_type %in% c("file", "directory"))) {
    input_val <- switch(prompt_type,
      file = tcltk::tk_choose.files(default = "", caption = "Select file", multi = FALSE),
      directory = tcltk::tk_choose.dir(default = getwd(), caption = "Select directory"),
      cli::cli_abort(cli::format_inline("Interactive selection is not supported for type {.emph {prompt_type}}."))
    )
    Sys.sleep(0.5) # Allow tk to print the closing message into the console
  } else if (input_val == "mock" && prompt_type == "file") {
    # Generate mock data and save to temp file
    box::use(
      artma / data / mock[create_mock_df],
      artma / libs / core / utils[get_verbosity]
    )

    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Generating mock data...")
    }

    # Create temp file in R temp directory
    temp_file <- tempfile(pattern = "artma-mock-data-", fileext = ".csv")

    # Generate mock data frame and save to temp file
    mock_df <- create_mock_df(
      with_file_creation = TRUE,
      file_path = temp_file
    )

    if (get_verbosity() >= 3) {
      cli::cli_alert_success("Mock data generated and saved to {.path {temp_file}}")
    }

    input_val <- temp_file
  }

  val_is_empty <- (!nzchar(input_val) || rlang::is_empty(input_val))

  if (val_is_empty) {
    if (!is.null(opt$default)) {
      return(opt$default)
    }
    if (isTRUE(opt$allow_na)) {
      return(NA)
    }
    cli::cli_abort("Required option {CONST$STYLES$OPTIONS$NAME(opt$name)} was left blank. Aborting.")
  }

  input_val
}

#' @title Resolve an option value
#' @description Resolve an option value, either using a user-provided value, a default value, or prompting the user.
#' @param opt [list] The option definition.
#' @param user_input [list] A named list of user-provided values.
#' @param is_interactive [logical(1)] Whether to prompt the user for missing/required values.
#' @keywords internal
resolve_option_value <- function(
    opt,
    user_input) {
  is_interactive <- interactive()

  if (opt$name %in% names(user_input)) {
    return(user_input[[opt$name]])
  }

  # 2) No user value, check default
  if (!is.null(opt$default)) {
    if (is_interactive && isTRUE(opt$confirm_default)) {
      return(prompt_user_for_option_value(opt))
    }
    return(opt$default)
  }

  # 3) No user value, no default
  if (is.null(opt$default)) {
    if (!is_interactive) {
      cli::cli_abort("Required option {CONST$STYLES$OPTIONS$NAME(opt$name)} not provided, and no default is available.")
    }
    return(prompt_user_for_option_value(opt))
  }

  cli::cli_abort("Unreachable code reached.")
}


#' @title Coerce an option value
#' @description Coerce an option value to the type declared in its template
#'   definition. Coercion is fail-loud: if the value cannot be represented as the
#'   expected type (a non-numeric in a numeric option, a fractional in an integer
#'   option, a value outside an enum's members), it aborts naming the option, the
#'   raw value, and the expected type instead of silently returning the original.
#' @param val [any] The value to coerce.
#' @param opt [list] The option definition.
#' `any` The coerced value.
#' @keywords internal
coerce_option_value <- function(val, opt) {
  box::use(
    artma / const[CONST],
    artma / libs / core / utils[get_verbosity],
    artma / options / utils[parse_template_enum_value]
  )

  # If the value is NULL, there's nothing to coerce
  if (is.null(val)) {
    return(val)
  }

  if (length(val) == 1 && is.na(val) && isTRUE(opt$allow_na)) {
    return(val)
  }

  enforce_na_allowed <- function(v) {
    if (any(is.na(v)) && !isTRUE(opt$allow_na)) {
      cli::cli_abort("Option {CONST$STYLES$OPTIONS$NAME(opt$name)} does not allow NA values.")
    }
  }

  abort_coercion <- function(expected_type) {
    cli::cli_abort(c(
      "Cannot coerce option {CONST$STYLES$OPTIONS$NAME(opt$name)} to {CONST$STYLES$OPTIONS$TYPE(expected_type)}.",
      "x" = "Raw value: {CONST$STYLES$OPTIONS$VALUE(val)}."
    ))
  }

  enforce_na_allowed(val)

  # Enumerations, e.g. "enum: red|blue|green": coerce to character and validate membership.
  if (startsWith(opt$type, "enum:")) {
    coerced_val <- as.character(val)
    valid_values <- parse_template_enum_value(opt$type)
    if (!all(coerced_val %in% valid_values)) {
      cli::cli_abort(c(
        "Option {CONST$STYLES$OPTIONS$NAME(opt$name)} must be one of {.emph {toString(valid_values)}}.",
        "x" = "Got: {CONST$STYLES$OPTIONS$VALUE(val)}."
      ))
    }
    return(coerced_val)
  }

  coerce_numeric <- function(expected_type, as_fun) {
    coerced <- suppressWarnings(as_fun(val))
    # Coercion that manufactures NA out of a non-NA value is a failure, not a
    # silent downgrade: surface it with the option name and raw value.
    if (any(is.na(coerced) & !is.na(val))) {
      abort_coercion(expected_type)
    }
    coerced
  }

  coerced_val <- switch(opt$type,
    character = as.character(val),
    integer = {
      if (!is.numeric(val)) abort_coercion("integer")
      non_na <- val[!is.na(val)]
      if (any(non_na != as.integer(non_na))) abort_coercion("integer")
      as.integer(val)
    },
    logical = coerce_numeric("logical", as.logical),
    numeric = coerce_numeric("numeric", as.numeric),
    list = as.list(val),
    val
  )

  if (isTRUE(opt$standardize) && opt$type == "character") {
    standard_val <- make.names(coerced_val)
    if (standard_val != coerced_val && !is.na(coerced_val)) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Option {CONST$STYLES$OPTIONS$NAME(opt$name)} does not allow non-standard values. Standardizing from {CONST$STYLES$OPTIONS$VALUE(val)} to {CONST$STYLES$OPTIONS$VALUE(standard_val)}."
        )
      }
    }
    coerced_val <- standard_val
  }

  enforce_na_allowed(coerced_val)
  coerced_val
}

#' @title Parse options from a template
#' @description Parse options from a YAML template file, with optional user input.
#' @param path *\[character\]* Full path to the YAML file containing the options.
#' @param user_input [list or NULL] A named list of user-supplied values for these options. If `NULL` or missing entries exist, the function will prompt the user via `readline()` (for required entries) or use defaults (for optional ones).
#' @param interactive [logical(1)] Whether to prompt the user (via `readline()`) for missing/required values.  Defaults to `TRUE`.
#' @param add_prefix [logical(1)] Whether to add a package prefix to all. Defaults to FALSE.
#' #' `list` A list of options
parse_options_from_template <- function(
    path,
    user_input = list(),
    interactive = TRUE,
    add_prefix = FALSE) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[assert_options_template_exists],
    artma / options / column_preprocessing[preprocess_column_mapping]
  )
  assert_options_template_exists(path)

  raw_template_options <- read_template(path)
  options_def <- flatten_template_options(raw_template_options)

  parsed_options <- list()
  column_name_prefix <- "data.colnames."

  # First pass: Resolve non-column-name options (especially data.source_path)
  for (opt in options_def) {
    if (!startsWith(opt$name, column_name_prefix)) {
      val <- resolve_option_value(opt, user_input)
      val <- coerce_option_value(val, opt)
      # We do not validate here, only after all options are parsed
      parsed_options[[opt$name]] <- val

      # Update user_input with resolved value for preprocessing
      user_input[[opt$name]] <- val
    }
  }

  # Now that data.source_path might be resolved, run column preprocessing
  user_input <- preprocess_column_mapping(user_input, options_def)

  # Second pass: Resolve column name options (should now be in user_input from preprocessing)
  for (opt in options_def) {
    if (startsWith(opt$name, column_name_prefix)) {
      val <- resolve_option_value(opt, user_input)
      val <- coerce_option_value(val, opt)
      # We do not validate here, only after all options are parsed
      parsed_options[[opt$name]] <- val
    }
  }

  # Possibly add a prefix to all names
  if (isTRUE(add_prefix)) {
    names(parsed_options) <- paste0(CONST$PACKAGE_NAME, ".", names(parsed_options))
  }

  parsed_options
}

box::export(
  coerce_option_value,
  collect_leaf_paths,
  flatten_template_options,
  flatten_user_options,
  get_template_defaults,
  get_option_defs,
  parse_options_from_template,
  parse_template,
  read_template
)
