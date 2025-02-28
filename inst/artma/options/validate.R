# Helper: Map the expected type from an option definition.
get_expected_type <- function(opt_def) {
  # If an explicit type is given, use that.
  if (!is.null(opt_def$type)) {
    return(opt_def$type)
  }
  # If action is store_true, assume logical.
  if (!is.null(opt_def$action) && opt_def$action == "store_true") {
    return("logical")
  }
  rlang::abort(glue::glue("Invalid template definition for the option '{opt_def}'. Could not determine the expected value type."))
}

#' Validate an options file against an options template.
#'
#' This function reads a YAML template and an options file, flattens both structures,
#' and then checks that:
#'  - Every option defined in the template is present in the options file.
#'  - The value for each option is of the correct type.
#'  - (Optionally) It warns about extra options in the file that are not defined in the template.
#'
#' For each problem found (missing option or type mismatch), an error message is printed.
#'
#' @param template_path [character] Path to the YAML file containing the options template.
#' @param options_path [character] Path to the YAML file containing the actual options.
#' @return Invisibly returns a list of error messages (empty if no errors).
#' @export
validate_options_file <- function(template_path, options_path) {
  # Load the YAML files
  if (!file.exists(template_path)) {
    stop(glue::glue("Template file '{template_path}' does not exist."))
  }
  if (!file.exists(options_path)) {
    stop(glue::glue("Options file '{options_path}' does not exist."))
  }

  template <- yaml::read_yaml(template_path)
  options_file <- yaml::read_yaml(options_path)

  # Flatten the template options definitions.
  # (Assumes your flatten_template_options function is in the environment)
  template_defs <- flatten_template_options(template)

  # Flatten the options file. This works whether the options file is nested or already flat.
  flat_options <- nested_to_flat(options_file)

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
  for (opt_name in names(flat_options)) {
    if (!any(vapply(template_defs, function(x) identical(x$dest, opt_name), logical(1)))) {
      warning(paste0(
        "Extraneous option: '", opt_name,
        "' is not defined in the template."
      ))
    }
  }

  # Print the summary in a lint-style format.
  if (length(errors) > 0) {
    cat("Validation errors found:\n")
    for (err in errors) {
      cat("  - ", err, "\n")
    }
  } else {
    cat("All options are valid.\n")
  }

  invisible(errors)
}

# --- Example improvements to existing utility functions ---
#
# 1. flatten_template_options: you might consider ensuring that every option definition has
#    a 'dest' (or at least a derived one) even if the YAML template omits it.
#
# 2. parse_options_from_template: since you now pass arguments directly rather than via the command
#    line, you could add a parameter (e.g., 'args') with a default value and possibly refactor the
#    check to simplify the logic. (For example, if args is not supplied, default to an empty vector.)
#
# 3. The nested_to_flat and flat_to_nested functions work well. Just be sure to add tests to confirm
#    that they handle edge cases (e.g., names that already include dots).
#
# Usage example:
# validate_options_file("path/to/options_template.yaml", "path/to/options_file.yaml")
