#' Retrieve a subset of options from the global options namespace.
#'
#' This function retrieves all options that match a specified prefix
#' from the global options list and returns them as a named list.
#' The prefix is assumed to represent a hierarchical grouping of options
#' (e.g., `x.y` for options like `x.y.z` or `x.y.a`).
#'
#' @param prefix A string representing the prefix of the options to retrieve.
#'               The prefix should match the hierarchical group (e.g., `x.y`).
#' @return A named list of options under the specified prefix, with the prefix removed from the names.
#' @examples
#' options(x.y.z = "value1", x.y.a = "value2", x.b = "value3")
#' get_option_group("x.y")
#' # Returns:
#' # $z
#' # [1] "value1"
#' # $a
#' # [1] "value2"
get_option_group <- function(prefix) {
  options <- options()
  group_keys <- grep(paste0("^", prefix, "\\."), names(options), value = TRUE)
  group <- stats::setNames(lapply(group_keys, getOption), gsub(paste0("^", prefix, "\\."), "", group_keys))
  return(group)
}

#' @title Remove Options by Prefix
#' @description This function removes all options from the R namespace whose names start with a specified prefix.
#' @param prefix A string representing the prefix of the options to remove.
#' @return `NULL`
remove_options_with_prefix <- function(prefix) {
  opts <- options()
  opts_to_remove <- names(opts)[startsWith(names(opts), prefix)]

  if (getOption("artma.verbose", 3) >= 4) {
    cli::cli_inform("Clearing the following options from the options namespace: {.emph {opts_to_remove}}")
  }

  if (length(opts_to_remove) == 0) {
    if (getOption("artma.verbose", 3) >= 3) {
      cli::cli_alert_info("No options found with the prefix: {prefix}")
    }
    return(invisible(NULL))
  }

  options(stats::setNames(rep(list(NULL), length(opts_to_remove)), opts_to_remove))

  invisible(NULL)
}

#' @title Flat to nested
#' @description Convert a list of flat options to a nested one
#' @param flat_option_list [list] A list of flat options
flat_to_nested <- function(flat_option_list) {
  if (!is.list(flat_option_list)) {
    cli::cli_abort("The options must be passed as a flat list.")
  }

  # Function to recursively insert values into a nested list based on keys
  insert_nested <- function(lst, keys, value) {
    key <- keys[1]
    if (length(keys) == 1) {
      lst[[key]] <- value
    } else {
      if (is.null(lst[[key]])) {
        lst[[key]] <- list()
      }
      lst[[key]] <- insert_nested(lst[[key]], keys[-1], value)
    }
    lst
  }

  nested_list <- list()
  for (full_key in names(flat_option_list)) {
    keys <- strsplit(full_key, ".", fixed = TRUE)[[1]] # Split the key by dots
    nested_list <- insert_nested(nested_list, keys, flat_option_list[[full_key]])
  }

  nested_list
}


#' @title Parse template enum value
#' @description Parse a template enum value
#' @param opt_type *\[character\]* A template enum value
#' @return *\[character\]* A vector of valid values
parse_template_enum_value <- function(opt_type) {
  if (!startsWith(opt_type, "enum:")) {
    cli::cli_abort("The option type must start with 'enum:'.")
  }
  enum_str <- sub("^enum:", "", opt_type)
  values <- strsplit(enum_str, "\\|(?=(?:[^']*'[^']*')*[^']*$)", perl = TRUE)[[1]]
  trimmed_values <- gsub("^\\s*'?|'?\\s*$", "", values)
  trimmed_values
}

#' @title Parse options file name
#' @description Parse a string into one that can be used as an options file name. If this fails, raise an error.
parse_options_file_name <- function(input_string) {
  str_out <- rlang::duplicate(input_string)

  if (getOption("artma.verbose", 3) >= 4) {
    cli::cli_inform("Parsing the following string into a user options file name: {.file {input_string}}")
  }

  tryCatch(
    {
      # Remove quotes
      str_out <- gsub("'", "", str_out, fixed = TRUE)
      str_out <- gsub('"', "", str_out, fixed = TRUE)

      # Remove trailing and leading whitespace
      str_out <- stringr::str_trim(str_out, side = "both")
    },
    error = function(e) {
      cli::cli_abort("There was an error parsing the following into a valid user options file name: {.emph {input_string}}")
    }
  )

  if (!grepl(".yaml$|.yml$", str_out)) {
    cli::cli_abort("Please provide the name of the options file with .yaml suffix. Got: {.emph {str_out}}.")
  }

  str_out
}

#' A helper function to map the expected type from an option definition.
get_expected_type <- function(opt_def) {
  # If an explicit type is given, use that.
  if (!is.null(opt_def$type)) {
    return(opt_def$type)
  }
  # If action is store_true, assume logical.
  if (!is.null(opt_def$action) && opt_def$action == "store_true") {
    return("logical")
  }
  cli::cli_abort("Invalid template definition for the option '{opt_def}'. Could not determine the expected value type.")
}

#' @title Validate option type
#' @description A helper function that checks if a value matches the expected type.
#'   Returns an error message if it does not.
#' @param val [any] The value to validate.
#' @param opt_type *\[character\]* The expected type of the value.
#' @param opt_name *\[character\]* The name of the option.
#' @param allow_na *\[logical\]* Whether the value is allowed to be NA or NULL.
#'   Defaults to FALSE.
#' `character` An error message if the value does not match the expected type, or NULL otherwise.
validate_option_value <- function(val, opt_type, opt_name, allow_na = FALSE) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[validate]
  )

  validate(is.character(opt_type), is.character(opt_name)) # 'allow_na' can be NULL

  # Helper function for uniform error formatting:
  format_error <- function(opt_name, expected_type, val) {
    cli::format_inline("Option {CONST$STYLES$OPTIONS$NAME(opt_name)} must be {CONST$STYLES$OPTIONS$TYPE(expected_type)}, got: {CONST$STYLES$OPTIONS$VALUE(val)}")
  }

  if (is.null(val) || (length(val) == 1 && is.na(val))) {
    if (!isTRUE(allow_na)) {
      return(cli::format_inline("Option {CONST$STYLES$OPTIONS$NAME(opt_name)} cannot be NULL or NA."))
    } else {
      return(NULL) # NA/NULL is allowed
    }
  }

  # Handle enumerations, e.g. "enum: red|blue|green"
  if (startsWith(opt_type, "enum:")) {
    valid_values <- parse_template_enum_value(opt_type)
    if (!val %in% valid_values) {
      return(
        cli::format_inline(
          "Option {CONST$STYLES$OPTIONS$NAME(opt_name)} must be one of {.emph {toString(valid_values)}}; got {CONST$STYLES$OPTIONS$VALUE(val)}."
        )
      )
    }
    return(NULL)
  }

  switch(opt_type,
    character = if (!is.character(val)) format_error(opt_name, "character", val),
    integer = if (!is.numeric(val)) format_error(opt_name, "numeric/integer", val),
    logical = if (!is.logical(val)) format_error(opt_name, "logical", val),
    numeric = if (!is.numeric(val)) format_error(opt_name, "numeric", val),
    NULL
  )
}

#' @title Validate user input
#' @description Validate user input to ensure it does not contain the package name prefix.
#' @param user_input [list] A list of user input.
#' @return `NULL`
validate_user_input <- function(user_input) {
  box::use(artma / const[CONST])

  pkg_name <- CONST$PACKAGE_NAME

  # None of the names should start with the package name
  has_pkg_prefix <- vapply(names(user_input), function(x) startsWith(x, pkg_name), logical(1))
  if (any(has_pkg_prefix)) {
    invalid_names <- names(user_input)[has_pkg_prefix] # nolint: unused_declared_object_linter.
    cli::cli_abort("Please provide the names of the options without the '{pkg_name}' prefix. Got: {.code {invalid_names}}.")
  }

  return(invisible(NULL))
}

#' @title Print options help text
#' @description Print options help text
#' @param help *\[character\]* The help text to print
#' @return `NULL`
#' @export
print_options_help_text <- function(help) {
  help_key <- cli::format_inline("{.strong Help}: ")
  help_body <- cli::format_inline(help)
  writeLines(paste0(help_key, help_body))
}


box::export(
  flat_to_nested,
  get_expected_type,
  get_option_group,
  parse_options_file_name,
  parse_template_enum_value,
  print_options_help_text,
  remove_options_with_prefix,
  validate_option_value,
  validate_user_input
)
