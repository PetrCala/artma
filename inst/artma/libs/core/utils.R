#' @title Get verbosity
#' @description Get the verbosity level
#' @return [integer] The verbosity level
#' @export
get_verbosity <- function() {
  box::use(artma / const[CONST])
  getOption("artma.verbose", CONST$DEFAULT_VERBOSITY)
}

#' @title Check whether R's data viewer can be opened
#' @description `utils::View()` hands the data off to the front-end's spreadsheet
#'   window. In a non-interactive session, or on a Unix system whose only viewer
#'   is the X11 data entry window and where no display is reachable, there is no
#'   window to attach to and R aborts the session instead of failing gracefully.
#'   Call this before `utils::View()` and fall back to printing when it is
#'   `FALSE`.
#' @return *\[logical\]* `TRUE` when `utils::View()` is safe to call.
#' @export
data_viewer_available <- function() {
  if (!interactive()) {
    return(FALSE)
  }
  # Examples and tests run in a subprocess of `R CMD check`. A viewer crash
  # there aborts the whole check rather than failing one expectation, so stay
  # on the printing fallback whenever a check is in progress, no matter how
  # the subprocess reports itself.
  if (!identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")) {
    return(FALSE)
  }
  if (identical(.Platform$OS.type, "windows")) {
    return(TRUE)
  }
  # Front-ends such as RStudio and Positron install their own viewer and never
  # reach the X11 data entry window.
  if (!identical(Sys.getenv("RSTUDIO"), "") || !identical(Sys.getenv("POSITRON"), "")) {
    return(TRUE)
  }
  if (identical(.Platform$GUI, "AQUA")) {
    return(TRUE)
  }
  isTRUE(unname(capabilities("X11"))) && !identical(Sys.getenv("DISPLAY"), "")
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
