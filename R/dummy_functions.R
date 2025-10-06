# This script serves as a placeholder for the functions that are used inside the internal 'inst/artma' folder logic, so that the R compiler can treat them as 'used' and not throw warnings about unused functions.

# nolint start: box_usage_linter.

#' @keywords internal
.dummy_use_here <- function() here::here("R", "dummy_functions.R")

#' @keywords internal
.dummy_use_stringr <- function() stringr::str_trim("")

#' @keywords internal
.dummy_use_rlang <- function() rlang::is_true(TRUE)

#' @keywords internal
.dummy_use_metafor <- function() metafor::rma(yi = 1:10, vi = 1:10)

#' @keywords internal
.dummy_use_purrr <- function() purrr::map_chr(1:10, ~.x)

#' @keywords internal
.dummy_use_memoise <- function() memoise::memoise(identity)

#' @keywords internal
.dummy_use_digest <- function() digest::digest("")


# nolint end: box_usage_linter.
