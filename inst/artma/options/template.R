#' Recursively flatten nested template options into a single list of option definitions with flattened destination names (e.g., x.y.z).
#' @keywords internal
flatten_template_options <- function(x, parent = NULL) {
  # Define a helper to recognize a final option definition.
  is_option_def <- function(e) {
    is.list(e) &&
      "long" %in% names(e)
  }

  flattened <- list()

  # If x itself is a list of final option definitions.
  if (is.list(x) && all(vapply(x, FUN = is_option_def, FUN.VALUE = logical(1)))) {
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
      flattened <- c(flattened, flatten_template_options(x[[name]], new_parent))
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


#' Parse an options template file into a list of options and return this list
#'
#' @param path [character] Full path to the YAML file containing the options.
#' @param args [vector(character)] Command line arguments to parse.
#' @param add_prefix [bool, optional] Whether to add a package prefix to all. Defaults to FALSE.
#' @returns [list] A list of options
#' @export
parse_options_from_template <- function(path, args, add_prefix = FALSE) {
  if (!file.exists(path)) {
    rlang::abort(glue::glue("Options file '{path}' does not exist."))
  }

  box::use(artma / const[CONST])

  args_is_character_vector <- is.vector(args) && all(vapply(args, FUN = is.character, FUN.VALUE = character(1)))
  args_is_empty <- length(args) == 0
  if (!(args_is_character_vector || args_is_empty)) {
    rlang::abort("Arguments must be a character vector or an empty character.")
  }

  raw_template_options <- yaml::read_yaml(path)

  # Use the new flatten_options that builds flattened dest values.
  options_def <- flatten_template_options(raw_template_options)
  parser <- build_parser(options_def)

  # Parse command line arguments with the built parser.
  parsed_options <- optparse::parse_args(parser, args = args)

  if (add_prefix) {
    # Prepare a list of options with names prefixed by "artma."
    # This will transform each element so that, for example, a parsed_args element
    # named "x.y.z" becomes an R option named "artma.x.y.z".
    parsed_options <- stats::setNames(parsed_options, paste0(CONST$PACKAGE_NAME, ".", names(parsed_options)))
  }

  parsed_options
}
