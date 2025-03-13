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
#' @return [NULL]
remove_options_with_prefix <- function(prefix) {
  opts <- options()
  opts_to_remove <- names(opts)[startsWith(names(opts), prefix)]

  logger::log_debug("Clearing the following options from the options namespace:")
  logger::log_debug(opts_to_remove)

  if (length(opts_to_remove) == 0) {
    logger::log_debug("No options found with the prefix: ", prefix)
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
    rlang::abort("The options must be passed as a flat list.")
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

#' @title Nested to flat
#' @description Convert a list of nested options to a flat one
#' @param nested [list] A list of nested options
#' @param parent_key [character, optional] Parent key for the nested options. Defaults to NULL.
#' @param sep [character, optional] Separator to use when concatenating the level names. Defaulst to '.'.
nested_to_flat <- function(nested, parent_key = NULL, sep = ".") {
  if (!is.list(nested)) {
    rlang::abort("The options must be passed as a nested list.")
  }

  flat <- list()

  for (name in names(nested)) {
    if (is.null(parent_key)) {
      new_key <- name
    } else {
      new_key <- paste(parent_key, name, sep = sep)
    }

    if (is.list(nested[[name]])) {
      flat <- c(flat, nested_to_flat(nested[[name]], new_key, sep))
    } else {
      flat[[new_key]] <- nested[[name]]
    }
  }

  flat
}

#' @title Parse options file name
#' @description Parse a string into one that can be used as an options file name. If this fails, raise an error.
parse_options_file_name <- function(input_string) {
  str_out <- rlang::duplicate(input_string)

  logger::log_debug(cli::format_inline("Parsing the following string into a user options file name: {.emph {input_string}}"))

  tryCatch(
    {
      # Remove quotes
      str_out <- gsub("'", "", str_out, fixed = TRUE)
      str_out <- gsub('"', "", str_out, fixed = TRUE)

      # Remove trailing and leading whitespace
      str_out <- stringr::str_trim(str_out, side = "both")
    },
    error = function(e) {
      rlang::abort(cli::format_inline("There was an error parsing the following into a valid user options file name: {.emph {input_string}}"))
    }
  )

  if (!grepl(".yaml$|.yml$", str_out)) {
    rlang::abort(cli::format_inline("Please provide the name of the options file with .yaml suffix. Got: {.emph {str_out}}."))
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
  rlang::abort(glue::glue("Invalid template definition for the option '{opt_def}'. Could not determine the expected value type."))
}

#' @title Validate option type
#' @description A helper function that checks if a value matches the expected type.
#'   Returns an error message if it does not.
#' @param val [any] The value to validate.
#' @param opt_type [character] The expected type of the value.
#' @param opt_name [character] The name of the option.
#' @param allow_na [logical] Whether the value is allowed to be NA or NULL.
#'   Defaults to FALSE.
#' @return [character] An error message if the value does not match the expected type, or NULL otherwise.
validate_option_value <- function(val, opt_type, opt_name, allow_na = FALSE) {
  box::use(artma / libs / validation[validate])

  validate(is.character(opt_type), is.character(opt_name)) # 'allow_na' can be NULL

  # Helper function for uniform error formatting:
  format_error <- function(opt_name, expected_type, val) {
    cli::format_inline("Option {cli::col_magenta(opt_name)} must be {cli::col_cyan(expected_type)}, got: {cli::col_green(val)}")
  }

  if (is.null(val) || (length(val) == 1 && is.na(val))) {
    if (!isTRUE(allow_na)) {
      return(cli::format_inline("Option {cli::col_magenta(opt_name)} cannot be NULL or NA."))
    } else {
      return(NULL) # NA/NULL is allowed
    }
  }

  # Handle enumerations, e.g. "enum: red|blue|green"
  if (startsWith(opt_type, "enum:")) {
    valid_values <- strsplit(sub("^enum:", "", opt_type), "\\|")[[1]]
    if (!val %in% valid_values) {
      return(
        cli::format_inline(
          "Option {cli::col_magenta(opt_name)} must be one of {.emph {toString(valid_values)}}; got {.val {val}}."
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


#' This is a public package method. For more information, see 'options.R::options.list'.
list_user_options_files <- function(options_dir = NULL, should_return_verbose_names = FALSE) {
  box::use(
    artma / paths[PATHS],
    artma / const[CONST]
  )
  options_dir <- options_dir %||% PATHS$DIR_USER_OPTIONS

  if (!dir.exists(options_dir)) {
    rlang::abort(cli::format_inline("The following options directory does not exist: {.path {options_dir}}"))
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
          logger::log_debug(cli::format_inline("Reading the options file {.path {file_name}}"))
          options_name <- yaml::read_yaml(file_name)$general$name
        },
        error = function(cond) {
          logger::log_warn(cli::format_inline("Failed to read the following options file: {.path {file}}"))
        }
      )
    } else {
      file_name
    }
    options_names <- append(options_names, options_name)
  }
  return(options_names)
}


box::export(
  flat_to_nested,
  get_expected_type,
  get_option_group,
  list_user_options_files,
  nested_to_flat,
  parse_options_file_name,
  remove_options_with_prefix,
  validate_option_value
)
