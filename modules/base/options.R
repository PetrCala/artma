box::use(
  yaml,
  base / paths[PATHS],
  base / const[CONST],
  libs / validation[assert, validate],
)

#' Count Unique Names in a Nested List
#'
#' This function counts the number of unique names in a nested list. If the list has only one level,
#' it returns the number of names in that list. For nested lists, it counts the number of named elements
#' across all levels, counting the names within sublists but not the sublist nodes themselves.
#'
#' @param lst [any] The element to check.
#' @param skip_null [logical] Whether to skip NULL elements when counting. Default is FALSE
#' @return An integer representing the count of unique names in the list.
#' @examples
#' count_unique_names(list(a = 1, b = list(c = 2, d = 3))) # Returns 3
#' count_unique_names(list(a = 1, b = list(c = 2, d = 3), e = list(f = 4, g = 5))) # Returns 5
#' count_unique_names(list(a = list(x = 1, y = 2), b = list(c = 3, d = 4))) # Returns 4
count_unique_names <- function(lst, skip_null = FALSE) {
  count <- 0
  if (!is.list(lst)) {
    return(1)
  }
  for (element in lst) {
    if (skip_null && is.null(element)) {
      next
    }
    if (is.list(element)) {
      count <- count + count_unique_names(element, skip_null = skip_null)
    } else {
      count <- count + 1
    }
  }
  return(count)
}

#' Recursively parse the YAML structure and set options_enum to the global options_enum namespace. Prepend each option with a custom prefix, separated by a period.
#'
#' @param options_list A list of option_enums
#' @param prefix A prefix to use for the option_enums
#' @return NULL
#' @example
#' set_prefixed_options(list(a = 1, b = list(c = 2, d = 3)), "prefix")
#' # Sets the following options_enum:
#' # prefix.a = 1
#' # prefix.b.c = 2
#' # prefix.b.d = 3
set_prefixed_options <- function(options_list, prefix) {
  for (name in names(options_list)) {
    value <- options_list[[name]]
    if (is.list(value)) {
      # If the value is a list, recursively process it
      set_prefixed_options(value, paste(prefix, name, sep = "."))
    } else {
      # Set the option with the full prefixed name
      option_name <- paste(prefix, name, sep = ".")
      R.utils::setOption(option_name, value)
    }
  }
}

#' Set all options_enum defined in the configuration YAML file into the global option_enums
#'
#' @param options_to_set [list] A list of options_enum to set to the global options_enum namespace
#' @param prefix [character] A prefix to use for the option_enums
#' @return NULL
#' @example
#' set_options(list(a = 1, b = list(c = 2, d = 3)), "prefix")
#' # Sets the following options_enum:
#' # prefix.a = 1
#' # prefix.b.c = 2
#' # prefix.b.d = 3
set_options <- function(options_to_set, prefix) {
  assert(is.list(options_to_set), "options_to_set must be a list.")
  assert(is.character(prefix), "prefix must be a character.")

  set_prefixed_options(options_to_set, prefix)

  expected_option_count <- count_unique_names(options_to_set, skip_null = TRUE)
  options_set <- options()[grep(paste0("^", prefix), names(options()))]

  assert(
    length(options_set) == expected_option_count,
    paste0("Failed to set custom options. Detected ", expected_option_count, " options_enum, but set ", length(options_set), ".")
  )
}

#' Validate the options_enum list against the options_enum enumeration
#'
#' @param option_list [list] The list of options_enum to validate
#' @param prefix [character] The prefix to use for the option_enums
#' @param verbose [logical] Whether to print verbose output. Default is FALSE.
#' @return NULL
validate_options <- function(option_list, prefix, verbose = FALSE) {
  for (path in names(OPTIONS_ENUM)) {
    # Split the path into components to traverse the list
    path_components <- unlist(strsplit(path, "\\."))

    # Traverse the list to get the value
    value <- option_list
    for (component in path_components) {
      if (!is.list(value) || !component %in% names(value)) {
        rlang::abort(paste("option_listuration error: path", path, "is missing in the option_listuration"))
      }
      value <- value[[component]]
    }

    option_info <- OPTIONS_ENUM[[path]]

    # Infer the type from allowed_values if type is not specified
    if (is.null(option_info$type) && !is.null(option_info$allowed_values)) {
      inferred_types <- unique(sapply(option_info$allowed_values, class))
      if (length(inferred_types) > 1) {
        rlang::abort("Multiple types detected in allowed_values. Please specify the type explicitly.")
      }
      option_info$type <- inferred_types
    }

    # Check if the value matches the required type
    if (!is.null(option_info$type) && class(value) != option_info$type) {
      rlang::abort(paste("Invalid type for", path, ". Expected:", option_info$type))
    }

    # Check if the value is in the allowed values (if specified)
    if (!is.null(option_info$allowed_values) && !value %in% option_info$allowed_values) {
      rlang::abort(paste("Invalid value for", path, ". Allowed values are:", paste(option_info$allowed_values, collapse = ", ")))
    }
  }

  if (verbose) {
    cat("All option values are valid.\n")
  }
}

#' Load options_enum from the configuration YAML file and set them to the global options_enum namespace
#'
#' @usage Call this function once at the beginning of the invocation script.
#' @return NULL
#' @export
load_options <- function() {
  options_enum <- yaml::read_yaml(PATHS$R_CONFIG_YAML)
  # validate_options(option_list = options_enum, prefix = CONST$PACKAGE_NAME)
  set_options(options_to_set = options_enum, prefix = CONST$PACKAGE_NAME)
}

#' Get an option value by name
#'
#' @param name [character] The name of the option to get
#' @return The value of the option
#' @example
#' get_option("dynamic_options.log_level") # "INFO"
#' @export
get_option <- function(name) {
  if (!name %in% names(OPTIONS_ENUM)) {
    rlang::abort(paste("Unknown option:", name))
  }
  option_name <- paste0(CONST$PACKAGE_NAME, ".", name)
  option <- R.utils::getOption(option_name)
  return(option)
}


#' Get a list of all options from an option group. If the group does not exist, an empty list is returned.
#'
#' @param group_name [character] The name of the option group
#' @return [list] A list of options from the group
#' @usage
#' get_options("dynamic_options") # list(log_level = "INFO", use_cache = TRUE, cache_dir = "/tmp")
#' @export
get_options <- function(group_name) {
  relevant_options <- grep(paste0("^", group_name), names(OPTIONS_ENUM), value = TRUE)
  if (length(relevant_options) == 0) {
    return(list())
  }
  options_values <- sapply(relevant_options, get_option)
  options_names <- gsub(paste0("^", group_name, "\\."), "", relevant_options)
  options_list <- setNames(options_values, options_names)
  return(options_list)
}
