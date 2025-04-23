#' @title Read template
#' @description Read a template YAML file and remove the temp block.
#' @param template_path *\[character\]* Path to the template YAML file.
#' @return A list of template options.
read_template <- function(template_path) {
  template <- yaml::read_yaml(template_path)
  template[["temp"]] <- NULL
  template
}

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

#' @title Collect leaf paths from template
#' @description Returns a character vector of fully-qualified option names
#' @param template_path *\[character\]* Path to the template YAML file
#' @return A character vector of fully-qualified option names
collect_leaf_paths <- function(template_path) {
  template <- read_template(template_path)
  defs <- flatten_template_options(template)
  vapply(defs, `[[`, character(1), "name")
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

    # ──► If we've reached a declared template leaf, take the whole value as-is
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

#' @title Print options help text
#' @description Print options help text
#' @param help *\[character\]* The help text to print
#' @keywords internal
print_options_help_text <- function(help) {
  help_key <- cli::format_inline("{.strong Help}: ")
  help_body <- cli::format_inline(help)
  writeLines(paste0(help_key, help_body))
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
    cli::cli_alert_warning(glue::glue(
      "Ignoring user-provided value for fixed option '{opt_name}'."
    ), call. = FALSE)
  }
  if (!is.null(opt$default)) {
    return(opt$default)
  } else if (is.null(opt_default)) {
    cli::cli_abort(glue::glue(
      "Required option '{opt_name}' is fixed, but no default is provided."
    ), call. = FALSE)
  } else {
    return(NULL) # Not required, no default
  }
}

#' @title Prompt user for a required value with no default
#' @description Prompt the user for a value, displaying the option name, type, and help.
#' @param opt [list] Option definition.
#' @keywords internal
prompt_user_for_option_value <- function(opt) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[assert]
  )

  assert(interactive(), "Running in a non-interactive mode. Cannot prompt for required option.")

  cli::cli_h1("Provide Option Value")
  cli::cli_text("{.strong Option name}: {CONST$STYLES$OPTIONS$NAME(opt$name)}")
  cli::cli_text("{.strong Type}: {CONST$STYLES$OPTIONS$TYPE(opt$type)}")
  if (!is.null(opt$default)) {
    cli::cli_text("{.strong Default}: {CONST$STYLES$OPTIONS$DEFAULT(opt$default)}")
  }

  if (!is.null(opt$help)) print_options_help_text(opt$help)

  prompt_type <- if (is.null(opt$prompt)) CONST$OPTIONS$DEFAULT_PROMPT_TYPE else opt$prompt

  if (prompt_type == CONST$OPTIONS$PROMPT_TYPES$FUNCTION) {
    if (is.null(opt$prompt_function)) {
      cli::cli_abort(cli::format_inline("Prompt function not provided for option {.strong {opt$name}}."))
    }
    box_import_str <- glue::glue("box::use(prompts = artma / options / prompts[{opt$prompt_function}])")
    tryCatch(
      {
        eval(parse(text = box_import_str))
        prompt_function <- get(opt$prompt_function, prompts)
      },
      error = function(e) {
        cli::cli_abort(cli::format_inline("Prompt function {.strong {opt$prompt_function}} not found."))
      }
    )
    return(prompt_function())
  }

  # nolint start: unused_declared_object_linter.
  base_msg <- cli::format_inline("Enter a value for {.strong {opt$name}}")
  choose_msg <- switch(prompt_type,
    "file" = cli::format_inline(" (or type in {.emph {'choose'}} to select a file interactively)"),
    "directory" = cli::format_inline(" (or type in {.emph {'choose'}} to select a directory interactively)"),
    "readline" = "",
    cli::cli_abort(cli::format_inline("Invalid prompt type {.emph {prompt_type}}."))
  )
  default_msg <- if (!is.null(opt$default)) cli::format_inline(" (or press {.code <Enter>} to accept default: {.strong {opt$default}})") else ""
  input_val <- readline(prompt = cli::format_inline("{base_msg}{choose_msg}{default_msg}: "))
  # nolint end: unused_declared_object_linter.

  if (input_val == "choose") {
    input_val <- switch(prompt_type,
      file = tcltk::tk_choose.files(default = "", caption = "Select file", multi = FALSE),
      directory = tcltk::tk_choose.dir(default = getwd(), caption = "Select directory"),
      cli::cli_abort(cli::format_inline("Interactive selection is not supported for type {.emph {prompt_type}}."))
    )
    Sys.sleep(0.5) # Allow tk to print the closing message into the console
  }

  val_is_empty <- (!nzchar(input_val) || rlang::is_empty(input_val))

  if (val_is_empty) {
    if (!is.null(opt$default)) {
      return(opt$default)
    } else if (isTRUE(opt$allow_na)) {
      return(NA)
    } else {
      cli::cli_abort(cli::format_inline(
        "Required option {CONST$STYLES$OPTIONS$NAME(opt$name)} was left blank. Aborting."
      ), call. = FALSE)
    }
  }

  return(input_val)
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
    if (is_interactive && isTRUE(opt$confirm_default)) {
      return(prompt_user_for_option_value(opt))
    } else {
      # Non-interactive => silently use default
      return(opt$default)
    }
  }

  # 3) No user value, no default
  if (is.null(opt$default)) {
    if (!is_interactive) {
      cli::cli_abort(glue::glue(
        "Required option '{opt$name}' not provided, and no default is available."
      ), call. = FALSE)
    } else {
      return(prompt_user_for_option_value(opt))
    }
  }

  cli::cli_abort("Unreachable code reached.")
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

  if (is.na(val) && isTRUE(opt$allow_na)) {
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
        cli::cli_abort(glue::glue(
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

  raw_template_options <- read_template(path)
  options_def <- flatten_template_options(raw_template_options)

  parsed_options <- list()
  for (opt in options_def) {
    val <- resolve_option_value(opt, user_input)
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
  collect_leaf_paths,
  flatten_template_options,
  flatten_user_options,
  parse_options_from_template,
  read_template
)
