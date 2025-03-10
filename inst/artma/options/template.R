#' @title Flatten nested template options
#' @description Recursively flatten nested template options into a single list of
#' option definitions with flattened destination names (e.g., x.y.z).
#' @param x [list] A list of nested template options.
#' @param parent [character] The parent path to the current list of options.
#' @return [list] A list of flattened option definitions.
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
        # Concatenate parent path with the current dest value.
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

#' @title Validate or coerce an option value
#' @description A helper function that validates or coerces an option value to the
#' expected type. If the value is not of the expected type, the function will attempt
#' to coerce it. If coercion is not possible, the function will throw an error.
#' @param val [any] The value to validate or coerce.
#' @param opt_type [character] The expected type of the value.
#' @param opt_name [character] The name of the option.
#' @return [any] The validated or coerced value.
#' @keywords internal
validate_or_coerce <- function(val, opt_type, opt_name) {
  if (opt_type == "character") {
    if (!is.character(val)) val <- as.character(val)
  } else if (opt_type == "integer") {
    if (!is.numeric(val)) {
      stop(glue::glue(
        "Option '{opt_name}' must be numeric/integer, got: {val}"
      ))
    }
    val <- as.integer(val)
  } else if (opt_type == "logical") {
    if (!is.logical(val)) {
      stop(glue::glue(
        "Option '{opt_name}' must be logical, got: {val}"
      ))
    }
    val <- as.logical(val)
  } else if (startsWith(opt_type, "enum:")) {
    # e.g. "enum: red|blue|green"
    valid_values <- strsplit(sub("^enum:", "", opt_type), "\\|")[[1]]
    if (!(val %in% valid_values)) {
      stop(glue::glue(
        "Option '{opt_name}' must be one of {toString(valid_values)}; got '{val}'."
      ))
    }
  }
  val
}


#' @title Parse options from a template
#' @description Parse options from a YAML template file, with optional user input.
#' @param path [character] Full path to the YAML file containing the options.
#' @param user_input [list or NULL]
#'   A named list of user-supplied values for these options. If `NULL` or missing
#'   entries exist, the function will prompt the user via `readline()` (for required
#'   entries) or use defaults (for optional ones).
#' @param interactive [logical(1)]
#'   Whether to prompt the user (via `readline()`) for missing/required values.
#'   Defaults to `TRUE`.
#' @param add_prefix [logical(1)] Whether to add a package prefix to all. Defaults to FALSE.
#' @return [list] A list of options
parse_options_from_template <- function(
    path,
    user_input = list(),
    interactive = TRUE,
    add_prefix = FALSE) {
  if (!file.exists(path)) {
    rlang::abort(glue::glue("Options file '{path}' does not exist."))
  }

  box::use(artma / const[CONST])

  raw_template_options <- yaml::read_yaml(path)

  # Use the new flatten_options that builds flattened dest values.
  options_def <- flatten_template_options(raw_template_options)

  # Build the final list of options that merges template definitions with user inputs.
  parsed_options <- list()
  for (opt in options_def) {
    opt_name <- opt$name
    opt_required <- isTRUE(opt$required)
    opt_default <- opt$default
    opt_type <- opt$type

    # Check if user has supplied this option in 'user_input'.
    if (!is.null(user_input[[opt_name]])) {
      # Use user-supplied value
      val <- user_input[[opt_name]]
    } else {
      # If user_input does not have it, check for a default or prompt
      if (!is.null(opt_default)) {
        val <- opt_default
      } else if (opt_required) {
        if (!interactive) {
          # If not interactive and no default, throw an error
          stop(glue::glue(
            "Required option '{opt_name}' not provided and no default is available."
          ), call. = FALSE)
        } else {
          # Possibly add: Show a hint or a question
          val <- readline(
            prompt = glue::glue("Please specify a value for '{opt_name}': ")
          )
          if (!nzchar(val)) {
            stop(glue::glue(
              "Required option '{opt_name}' was left blank. Aborting."
            ), call. = FALSE)
          }
        }
      } else {
        val <- NULL # Not required, no default
      }
    }

    val <- validate_or_coerce(val, opt_type, opt_name) # Type validation

    parsed_options[[opt_name]] <- val
  }

  if (isTRUE(add_prefix)) {
    # Prepare a list of options with names prefixed by "artma."
    # This will transform each element so that, for example, a parsed_args element
    # named "x.y.z" becomes an R option named "artma.x.y.z".
    names(parsed_options) <- paste0(CONST$PACKAGE_NAME, ".", names(parsed_options))
  }

  parsed_options
}

box::export(
  flatten_template_options,
  parse_options_from_template
)
