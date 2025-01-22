#' Recursively flatten nested options into a single list of option definitions
#' @keywords internal
flatten_options <- function(x) {
  # If `x` is a list of entries where each entry contains a `short` field,
  # we consider this a list of final option definitions.
  is_option_def <- function(e) {
    is.list(e) && "short" %in% names(e) && "long" %in% names(e)
  }

  # If x itself is a list of options
  if (is.list(x) && all(sapply(x, is_option_def))) {
    return(x)
  }

  # Otherwise, x is expected to be a named list of subcategories
  flattened <- list()
  if (is.list(x)) {
    for (name in names(x)) {
      flattened <- c(flattened, flatten_options(x[[name]]))
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

  args_is_character_vector <- is.vector(args) && all(sapply(args, is.character))
  args_is_empty <- length(args) == 0
  if (!(args_is_character_vector || args_is_empty)) {
    rlang::abort("Arguments must be a character vector or an empty character.")
  }

  raw_options <- yaml::read_yaml(path)
  options_def <- flatten_options(raw_options)
  parser <- build_parser(options_def)

  args <- optparse::parse_args(parser, args = args)

  options(artma = args)
}
