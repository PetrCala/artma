# This script serves as a placeholder for the functions that are used inside the internal 'inst/artma' folder logic, so that the R compiler can treat them as 'used' and not throw warnings about unused functions.

# nolint start: box_usage_linter, unused_declared_object_linter.

#' @keywords internal
.dummy_use_here <- function() here::here("R", "dummy_functions.R")

#' @keywords internal
.dummy_use_stringr <- function() stringr::str_trim("")

#' @keywords internal
.dummy_use_rlang <- function() rlang::is_true(TRUE)

# nolint end: box_usage_linter, unused_declared_object_linter.
