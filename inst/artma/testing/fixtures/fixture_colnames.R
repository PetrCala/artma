#' @title With custom colnames
#' @description For a duration of the test, set the custom colnames to the options namespace so that any calls to 'get_option_group("artma.data.colnames")' return the provided mapping.
#' @param colnames_map *[list]* A list of column names to set.
#' @return *[function]* A function that restores the original colnames after the test.
with_custom_colnames <- function(colnames_map) {
  if (!is.list(colnames_map)) {
    cli::cli_abort("The provided colnames map must be a list.")
  }
  if (length(colnames_map) == 0) {
    cli::cli_abort("The provided colnames map must not be empty.")
  }
  opt_list <- stats::setNames(
    lapply(names(colnames_map), function(name) colnames_map[[name]]),
    paste0("artma.data.colnames.", names(colnames_map))
  )
  options(opt_list)
  withr::defer(options(opt_list), envir = parent.frame())
  invisible(NULL)
}

box::export(with_custom_colnames)
