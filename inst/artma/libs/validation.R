#' Validate Conditions
#'
#' This function validates input conditions. It checks that each argument is
#' a single logical value (TRUE or FALSE).
#' If any condition is invalid or does not hold, the function aborts with an
#' appropriate error message including the failed condition and a backtrace.
#'
#' @param ... Any number of logical conditions.
#'
#' @return NULL. The function is called for its side effects.
#' @export
#'
#' @examples
#' validate(1 == 1, 2 == 2, is.function(print))
#' # The following examples will abort with an error and print a backtrace
#' # validate(FALSE)
#' # validate(TRUE, FALSE, FALSE)
#' # validate("not a condition")
#' @export
validate <- function(...) {
  withr::with_options(
    rlang_backtrace_on_error = "full",
    error = rlang::entrace
  )

  conditions <- list(...)
  conditions_expr <- as.list(substitute(list(...)))[-1]

  # Validate each condition
  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]
    cond_expr <- deparse(conditions_expr[[i]])
    if (!is.logical(cond) || length(cond) != 1) {
      rlang::abort(
        message = paste("Condition must be a single logical value (TRUE or FALSE):", cond_expr),
        .subclass = "validation_error"
      )
    }
    if (!cond) {
      rlang::abort(
        message = paste("Condition did not hold:", cond_expr),
        .subclass = "validation_error"
      )
    }
  }
}

#' Check that a data frame contains specific columns
#'
#' @param df [data.frame] The data frame to check
#' @param columns [vector[character]] A set of columns to check
#' @return NULL Checks that the columns exist in the data frame
#' @example
#' validate_columns(df, c("effect", "se"))
#' @export
validate_columns <- function(df, columns) {
  if (!is.data.frame(df)) {
    rlang::abort("'df' must be a data frame.")
  }
  if (!is.character(columns)) {
    rlang::abort("'columns' must be a character vector")
  }

  if (!all(columns %in% colnames(df))) {
    rlang::abort(glue::glue_collapse("Invalid column names:", glue::glue_collapse(colnames(df), sep = ", ")), "Expected to contain:", paste(columns, sep = ", "))
  }
}


#' Assert Conditions
#'
#' This function asserts that a condition is TRUE. If the condition is FALSE,
#' the function aborts with an appropriate error message including the failed
#' condition and a backtrace.
#'
#' @param condition_to_validate [logical] The condition to validate.
#' @param error_message [character(1), optional] The error message to display if the
#'  condition is FALSE.
#'
#' @return NULL. The function is called for its side effects.
#' @export
assert <- function(condition_to_validate, error_message = NULL) {
  if (is.null(error_message)) {
    error_message <- paste("Assertion failed:", deparse(substitute(condition_to_validate)))
  }
  if (!condition_to_validate) {
    rlang::abort(
      message = error_message,
      .subclass = "assertion_error"
    )
  }
}

#' Check if x is a vector and either empty or all characters
#'
#' @param x [any] The object to check
#' @param throw_error [logical] Whether to throw an error if the check fails
#' @return boolean indicating whether or not x is a character vector or empty
#' @export
is_char_vector_or_empty <- function(x, throw_error = FALSE) {
  is_empty <- is.vector(x) && (length(x) == 0 || is.character(x))
  if (throw_error && !is_empty) {
    rlang::abort("The object is not a character vector or empty")
  }
  return(is_empty)
}
