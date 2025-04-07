#' @title Flatten nested template options
#' @description Recursively flatten nested template options into a single list of
#' option definitions with flattened destination names (e.g., x.y.z).
#' @param x [list] A list of nested template options.
#' @param parent *\[character\]* The parent path to the current list of options.
#' `list` A list of flattened option definitions.
#' @export
flatten_template_options <- function(x, parent = NULL) {
  # A helper function to recognize a final option definition.
  is_option_def <- function(e) {
    is.list(e) &&
      all(c("name", "type") %in% names(e))
  }

  flattened <- list()

  # If x itself is a list of final option definitions.
  if (is.list(x) && all(vapply(x, FUN = is_option_def, FUN.VALUE = logical(1)))) {
    for (i in seq_along(x)) {
      # If there's a parent path, update the destination.
      if (!is.null(parent)) {
        # Concatenate parent path with the current name value.
        x[[i]]$name <- paste(parent, x[[i]]$name, sep = ".")
      }
      flattened[[length(flattened) + 1]] <- x[[i]]
    }
    return(flattened)
  }

  # If not a list of option definitions, x should be a list of subcategories.
  if (is.list(x)) {
    for (name in names(x)) {
      # Build the new parent path by appending the current name.
      new_parent <- if (is.null(parent)) name else paste(parent, name, sep = ".")
      flattened <- c(flattened, flatten_template_options(x[[name]], new_parent))
    }
  }

  flattened
}

#' @title Resolve a fixed option
#' @description Resolve a fixed option, either using a default value or throwing an error if no default is provided.
#' @param opt [list] Option definition.
#' @param user_input [list] A named list of user-provided values.
#' `any` The resolved value for the fixed option.
#' @keywords internal
resolve_fixed_option <- function(opt, user_input) {
  if (!is.null(user_input[[opt$name]])) {
    if (user_input[[opt$opt_name]] == opt$default) {
      return(opt$default)
    }
    # User tried to set a value for a fixed option to a non-default value
    warning(glue::glue(
      "Ignoring user-provided value for fixed option '{opt_name}'."
    ), call. = FALSE)
  }
  if (!is.null(opt$default)) {
    return(opt$default)
  } else if (is.null(opt_default)) {
    stop(glue::glue(
      "Required option '{opt_name}' is fixed, but no default is provided."
    ), call. = FALSE)
  } else {
    return(NULL) # Not required, no default
  }
}

#' @title Prompt user for a value with default
#' @description Prompt the user for a value, displaying the option name, type, help, and default value.
#' @param opt [list] Option definition.
#' `any` The user-provided value or the default value.
#' @keywords internal
prompt_user_with_default <- function(opt) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[assert]
  )

  assert(interactive(), "Running in a non-interactive mode. Cannot prompt for required option.")

  cli::cli_h1("Provide Option Value")
  cli::cli_text("{.strong Option name}: {CONST$STYLES$OPTIONS$NAME(opt$name)}")
  cli::cli_text("{.strong Type}: {CONST$STYLES$OPTIONS$TYPE(opt$type)}")

  if (!is.null(opt$help)) {
    cli::cli_text("{.strong Help}: {.emph {gsub('%default', opt$default, opt$help)}}")
  }

  cli::cli_text("{.strong Default}: {CONST$STYLES$OPTIONS$DEFAULT(opt$default)}")

  input_val <- readline(
    # Here we assume that for opts with defaults, there is no need to choose interactively
    # This includes, for example, selecting folders, file paths, etc.
    prompt = cli::format_inline("Enter value (or press {.code <Enter>} to accept default): ")
  )


  if (nzchar(input_val)) {
    return(input_val)
  } else {
    return(opt$default)
  }
}

#' @title Prompt user for a required value with no default
#' @description Prompt the user for a value, displaying the option name, type, and help.
#' @param opt [list] Option definition.
#' `any` The user-provided value.
#' @keywords internal
prompt_user_required_no_default <- function(opt) { # nolint: object_length_linter.
  box::use(
    artma / const[CONST],
    artma / libs / utils[is_empty],
    artma / libs / validation[assert]
  )

  assert(interactive(), "Running in a non-interactive mode. Cannot prompt for required option.")

  cli::cli_h1("Option Value Required")
  cli::cli_text("{.strong Option name}: {CONST$STYLES$OPTIONS$NAME(opt$name)}")
  cli::cli_text("{.strong Type}: {CONST$STYLES$OPTIONS$TYPE(opt$type)}")

  if (!is.null(opt$help)) {
    cli::cli_text("{.strong Help}: {.emph {opt$help}}")
  }

  prompt_msg <- switch(opt$prompt, # nolint: unused_declared_object_linter.
    file = cli::format_inline(" (or enter {.emph {'choose'}} to select a file interactively)"),
    directory = cli::format_inline(" (or enter {.emph {'choose'}} to select a directory interactively)"),
    NULL = "",
    cli::cli_abort(cli::format_inline("Invalid prompt type {.emph {prompt_type}}."))
  )

  input_val <- readline(
    prompt = cli::format_inline("Enter a value for {.strong {opt$name}}{prompt_msg}: ")
  )

  if (input_val == "choose") {
    input_val <- switch(opt$prompt,
      file = tcltk::tk_choose.files(default = "", caption = "Select file", multi = FALSE),
      directory = tcltk::tk_choose.dir(default = getwd(), caption = "Select directory"),
      cli::cli_abort(cli::format_inline("Interactive selection is not supported for type {.emph {prompt_type}}."))
    )
  }

  if ((!nzchar(input_val) || is_empty(input_val)) && !isTRUE(opt$allow_na)) {
    stop(cli::format_inline(
      "Required option {CONST$STYLES$OPTIONS$NAME(opt$name)} was left blank. Aborting."
    ), call. = FALSE)
  }

  return(input_val)
}

#' @title Resolve an option value
#' @description Resolve an option value, either using a user-provided value, a default value, or prompting the user.
#' @param opt [list] The option definition.
#' @param user_input [list] A named list of user-provided values.
#' @param is_interactive [logical(1)] Whether to prompt the user for missing/required values.
#' @param should_ask_for_default_options [logical(1)] Whether to prompt the user for options that have a default value. Defaults to FALSE.
#' `any` The resolved value for the option.
#' @keywords internal
resolve_option_value <- function(
    opt,
    user_input,
    is_interactive = interactive(),
    should_ask_for_default_options = FALSE) {
  if (isTRUE(opt$fixed)) {
    return(resolve_fixed_option(opt, user_input))
  }

  if (opt$name %in% names(user_input)) {
    # 1) If user explicitly provided a value, just return it
    return(user_input[[opt$name]])
  }

  # 2) No user value, check default
  if (!is.null(opt$default)) {
    # If interactive, prompt to allow override (optional)
    if (is_interactive && should_ask_for_default_options) {
      return(prompt_user_with_default(opt))
    } else {
      # Non-interactive => silently use default
      return(opt$default)
    }
  }

  # 3) No user value, no default
  if (is.null(opt$default)) {
    if (!is_interactive) {
      stop(glue::glue(
        "Required option '{opt$name}' not provided, and no default is available."
      ), call. = FALSE)
    } else {
      return(prompt_user_required_no_default(opt))
    }
  }

  # 4) Not required, no default => NULL
  return(NULL)
}


#' @title Coerce an option value
#' @description A helper function that attempts to coerce an option value to the correct type. If it fails, it passes the value as is.
#' @param val [any] The value to validate or coerce.
#' @param opt [list] The option definition.
#' `any` The coerced value.
#' @keywords internal
coerce_option_value <- function(val, opt) {
  # If the value is NULL, there's nothing to coerce
  if (is.null(val)) {
    return(val)
  }

  # Enumerations, e.g. "enum: red|blue|green", return as is
  if (startsWith(opt$type, "enum:")) {
    return(val)
  }

  tryCatch(
    {
      coerced_val <- switch(opt$type,
        character = as.character(val),
        integer = as.integer(val),
        logical = as.logical(val),
        numeric = as.numeric(val),
        list = as.list(val),
        val
      )
      # In come invalid cases, the coercion will return NA.
      # We re-throw an error and catch it to return the original value.
      # This makes it easier to find the invalid value afterwards.
      if (is.na(coerced_val) && !isTRUE(opt$allow_na)) {
        stop(glue::glue(
          "Option '{opt_name}' does not allow NA values."
        ), call. = FALSE)
      }
      coerced_val
    },
    error = function(e) val # Return the original value if coercion fails
  )
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
    artma / libs / validation[assert_options_template_exists]
  )
  assert_options_template_exists(path)

  raw_template_options <- yaml::read_yaml(path)

  options_def <- flatten_template_options(raw_template_options)

  parsed_options <- list()
  for (opt in options_def) {
    val <- resolve_option_value(opt, user_input, interactive())
    val <- coerce_option_value(val, opt)
    # We do not validate here, only after all options are parsed
    parsed_options[[opt$name]] <- val
  }

  # Possibly add a prefix to all names
  if (isTRUE(add_prefix)) {
    names(parsed_options) <- paste0(CONST$PACKAGE_NAME, ".", names(parsed_options))
  }

  parsed_options
}

box::export(
  flatten_template_options,
  parse_options_from_template
)
