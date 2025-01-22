#' Recursively flatten nested options into a single list of option definitions
#' with flattened destination names (e.g., x.y.z).
#' @keywords internal
flatten_options <- function(x, parent = NULL) {
  # Define a helper to recognize a final option definition.
  is_option_def <- function(e) {
    is.list(e) && "short" %in% names(e) && "long" %in% names(e)
  }

  flattened <- list()

  # If x itself is a list of final option definitions.
  if (is.list(x) && all(sapply(x, is_option_def))) {
    for (i in seq_along(x)) {
      # If there's a parent path, update the destination.
      if (!is.null(parent)) {
        # Concatenate parent path with the current dest value.
        # If dest isn't already provided, we might derive it from the long option.
        base_dest <- if (!is.null(x[[i]]$dest)) x[[i]]$dest else sub("^--", "", x[[i]]$long)
        x[[i]]$dest <- paste(parent, base_dest, sep = ".")
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
      flattened <- c(flattened, flatten_options(x[[name]], new_parent))
    }
  }

  flattened
}

#' Build an optparse parser from a list of option definitions
#' @keywords internal
build_parser <- function(options_def) {
  parser <- optparse::OptionParser()
  for (opt in options_def) {
    parser <- optparse::add_option(
      parser,
      c(opt$short, opt$long),
      action  = opt$action,
      default = opt$default,
      type    = opt$type,
      dest    = opt$dest,
      help    = opt$help,
      metavar = if (!is.null(opt$metavar)) opt$metavar else NULL
    )
  }
  parser
}

#' Load options from a YAML file and command line arguments. Store the options in the global environment.
#' @param path [character] Full path to the YAML file containing the options.
#' @param args [vector(character)] Command line arguments to parse.
#' @export
load_options <- function(path, args) {
  if (!file.exists(path)) {
    rlang::abort(glue::glue("Options file '{path}' does not exist."))
  }

  box::use(artma / const[CONST])

  args_is_character_vector <- is.vector(args) && all(sapply(args, is.character))
  args_is_empty <- length(args) == 0
  if (!(args_is_character_vector || args_is_empty)) {
    rlang::abort("Arguments must be a character vector or an empty character.")
  }

  raw_options <- yaml::read_yaml(path)
  # Use the new flatten_options that builds flattened dest values.
  options_def <- flatten_options(raw_options)
  parser <- build_parser(options_def)

  # Parse command line arguments with the built parser.
  parsed_args <- optparse::parse_args(parser, args = args)

  # Prepare a list of options with names prefixed by "artma."
  # This will transform each element so that, for example, a parsed_args element
  # named "x.y.z" becomes an R option named "artma.x.y.z".
  prefixed_options <- stats::setNames(parsed_args, paste0(CONST$PACKAGE_NAME, ".", names(parsed_args)))

  options(prefixed_options)
}

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
  group <- setNames(lapply(group_keys, getOption), gsub(paste0("^", prefix, "\\."), "", group_keys))
  return(group)
}
