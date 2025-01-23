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
    keys <- strsplit(full_key, "\\.")[[1]] # Split the key by dots
    nested_list <- insert_nested(nested_list, keys, flat_option_list[[full_key]])
  }

  nested_list
}
