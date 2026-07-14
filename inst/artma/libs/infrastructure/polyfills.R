#' @title Polyfills for Removed Dependencies
#' @description
#' Custom implementations of functions from removed dependencies to reduce
#' the package's dependency footprint. This module replaces functionality from:
#' - stringr (string manipulation)
#' - purrr (functional programming)
#'
#' All functions maintain API compatibility with the original packages.
#'
#' @name polyfills

# String manipulation (stringr replacements) ----

#' Remove first match from a string
#' @description Replacement for stringr::str_remove()
#' @param string Input vector
#' @param pattern Pattern to remove
#' @return Character vector with pattern removed
#' @export
str_remove <- function(string, pattern) {
  sub(pattern, "", string, perl = TRUE)
}

# Functional programming (purrr replacements) ----

#' Map over a vector and return character results
#' @description Replacement for purrr::map_chr()
#' @param .x A list or atomic vector
#' @param .f A function, formula, or vector
#' @param ... Additional arguments passed to .f
#' @return Character vector
#' @export
map_chr <- function(.x, .f, ...) {
  if (is.character(.f) && length(.f) == 1) {
    # Handle name extraction: map_chr(list, "name")
    vapply(.x, function(x) x[[.f]], character(1), USE.NAMES = FALSE)
  } else if (is.function(.f)) {
    vapply(.x, .f, character(1), ..., USE.NAMES = FALSE)
  } else {
    stop("`.f` must be a function or string") # nolint
  }
}

#' Map over a vector and return logical results
#' @description Replacement for purrr::map_lgl()
#' @param .x A list or atomic vector
#' @param .f A function or formula
#' @param ... Additional arguments passed to .f
#' @return Logical vector
#' @export
map_lgl <- function(.x, .f, ...) {
  # Handle formula notation (purrr-style)
  if (inherits(.f, "formula")) {
    formula_env <- environment(.f)
    formula_body <- .f[[2]]
    .f <- function(.x) {
      # Support both . and .x notation
      eval(formula_body, envir = list(.x = .x, . = .x), enclos = formula_env)
    }
  }

  if (is.function(.f)) {
    vapply(.x, .f, logical(1), ..., USE.NAMES = FALSE)
  } else {
    stop("`.f` must be a function or formula") # nolint
  }
}

#' Keep elements that satisfy a predicate
#' @description Replacement for purrr::keep()
#' @param .x A list or atomic vector
#' @param .p A predicate function or formula
#' @param ... Additional arguments passed to .p
#' @return Filtered vector
#' @export
keep <- function(.x, .p, ...) {
  # Handle formula notation (purrr-style)
  if (inherits(.p, "formula")) {
    # Convert formula ~expr to function(x) expr
    formula_env <- environment(.p)
    formula_body <- .p[[2]]
    # Replace .x with x in the formula
    .p <- function(x) {
      # Evaluate the formula body with x as the argument
      # Support both . and .x notation
      eval(formula_body, envir = list(.x = x, . = x), enclos = formula_env)
    }
  }
  Filter(.p, .x, ...)
}
