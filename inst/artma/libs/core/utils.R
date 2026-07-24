#' @title Get verbosity
#' @description Get the verbosity level
#' @return [integer] The verbosity level
#' @export
get_verbosity <- function() {
  box::use(artma / const[CONST])
  getOption("artma.verbose", CONST$DEFAULT_VERBOSITY)
}

#' @title Get an option, treating NULL and scalar NA as unset
#' @description `getOption()` wrapper for options whose template default is
#'   `.na` (`allow_na`): those are loaded into `options()` as a literal `NA`,
#'   which `getOption()` returns instead of the fallback. A stored `NULL` or
#'   length-one `NA` falls through to `default`; longer vectors are returned
#'   as-is even when they contain `NA`s.
#' @param name *\[character\]* Full option name, e.g. `"artma.data.na_handling"`.
#' @param default The value to return when the option is unset, `NULL`, or a
#'   scalar `NA`.
#' @return The option value, or `default`.
#' @export
opt_or <- function(name, default = NULL) {
  value <- getOption(name, default = default)
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(default)
  }
  value
}
