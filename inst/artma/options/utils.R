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

  logger::log_debug(glue::glue("Parsing the following string into a user options file name: {input_string}"))

  tryCatch(
    {
      # Remove quotes
      str_out <- gsub("'", "", str_out, fixed = TRUE)
      str_out <- gsub('"', "", str_out, fixed = TRUE)

      # Remove trailing and leading whitespace
      str_out <- stringr::str_trim(str_out, side = "both")
    },
    error = function(e) {
      rlang::abort(glue::glue("There was an error parsing the following into a valid user options file name: {input_string}"))
    }
  )

  if (!grepl(".yaml$|.yml$", str_out)) {
    rlang::abort(glue::glue("Please provide the name of the options file with .yaml suffix. Got: {options_file_name}."))
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
  # Helper function for uniform error formatting:
  format_error <- function(opt_name, expected_type, val) {
    glue::glue("Option '{opt_name}' must be {expected_type}, got: {val}")
  }

  if (is.null(val) || (length(val) == 1 && is.na(val))) {
    if (!isTRUE(allow_na)) {
      return(glue::glue("Option '{opt_name}' cannot be NULL or NA."))
    } else {
      return(NULL) # NA/NULL is allowed
    }
  }

  # Handle enumerations, e.g. "enum: red|blue|green"
  if (startsWith(opt_type, "enum:")) {
    valid_values <- strsplit(sub("^enum:", "", opt_type), "\\|")[[1]]
    if (!val %in% valid_values) {
      return(
        glue::glue(
          "Option '{opt_name}' must be one of {toString(valid_values)}; got '{val}'."
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


#' @title Ask for options file name
#' @description Ask the user to input a name of an options file. Clean the user's input and return it as a string.
#' @param should_clean [logical, optional] Whether to clean the input string. Defaults to TRUE
#' @param prompt [character, optional] The prompt to use. Defaults to a generic prompt.
#' @return [character] The options file name.
ask_for_options_file_name <- function(should_clean = TRUE, prompt = NULL) {
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }

  prompt <- prompt %||% "Please provide the name for your options file, including the .yaml suffix: "
  options_file_name <- readline(prompt = prompt)

  if (should_clean) {
    options_file_name <- parse_options_file_name(options_file_name)
  }

  return(options_file_name)
}


#' @title Ask for an existing options file name
#' @description Prompt the user to select from an existing list of user options files. Return the name of the selected file as a character.
#' @param options_dir [character, optional] Name of the directory to list the files from. Defaults to NULL.
#' @param prompt [character, optional] The prompt to use when asking for the user options file name. Defaults to NULL.
#' @return [character] Name of the selected file.
ask_for_existing_options_file_name <- function(options_dir = NULL, prompt = NULL) { # nolint: object_length_linter.
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(artma / libs / utils[is_empty])

  prompt <- prompt %||% "Please select the user options file name you would like to use."

  user_options_file_names <- list_user_options_files(options_dir = options_dir)
  selected_file_name <- utils::select.list(
    title = prompt,
    choices = user_options_file_names
  )
  if (is_empty(selected_file_name)) {
    stop("No user options file was selected. Aborting...")
  }
  return(selected_file_name)
}


box::export(
  ask_for_options_file_name,
  ask_for_existing_options_file_name,
  flat_to_nested,
  get_expected_type,
  get_option_group,
  list_user_options_files,
  nested_to_flat,
  parse_options_file_name,
  validate_option_value
)
