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
#'
#' @export
get_option_group <- function(prefix) {
  options <- options()
  group_keys <- grep(paste0("^", prefix, "\\."), names(options), value = TRUE)
  group <- stats::setNames(lapply(group_keys, getOption), gsub(paste0("^", prefix, "\\."), "", group_keys))
  return(group)
}

#' @title Flat to nested
#' @description Convert a list of flat options to a nested one
#' @param flat_option_list [list] A list of flat options
#' @export
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
#' @export
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
#' @export
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
