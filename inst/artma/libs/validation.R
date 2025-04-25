#' Parse a Condition Expression
#'
#' This helper function parses an expression of the form \code{is.TYPENAME(obj)},
#' e.g. \code{is.character(my_var)}. If recognized, it returns a more descriptive
#' message, indicating what the user provided and what was expected. Otherwise,
#' it returns a generic message based on the function name (e.g. "is.foo").
#'
#' @param cond_expr An expression or character string representing the condition
#'   (e.g. \code{substitute(is.character(obj_name))}).
#' @param env The environment to evaluate in. By default uses the parent frame.
#'
#' @return *\[character(1)]* A message indicating the expected type and the actual value.
#'
#' @examples
#' parse_condition(quote(is.character(my_var)))
#' parse_condition(quote(is.logical(x)))
#' parse_condition(quote(is.something_else(obj)))
#'
#' @keywords internal
parse_condition <- function(cond_expr, env = parent.frame()) {
  # Note: 'cond_expr' might already be a call if using e.g. substitute(...)
  if (is.character(cond_expr)) {
    cond_call <- tryCatch(rlang::parse_expr(cond_expr), error = function(e) NULL)
    if (is.null(cond_call)) {
      return(glue::glue("Condition did not hold: {cond_expr}"))
    }
  } else {
    cond_call <- cond_expr
  }

  if (!rlang::is_call(cond_call)) {
    return(glue::glue("Condition did not hold: {deparse(cond_expr)}"))
  }

  # Extract the function name and argument
  fn_name <- as.character(cond_call[[1]]) # e.g. "is.character"
  arg_expr <- cond_call[[2]] # e.g. "my_var" or a literal "1"
  arg_label <- deparse(arg_expr) # text version of the argument

  # Attempt to evaluate the argument in 'env' for a "Got: …" message
  # There's no guarantee that 'arg_expr' is a symbol, might be a literal, so we wrap in tryCatch.
  arg_value <- tryCatch(
    eval(arg_expr, envir = env),
    error = function(e) arg_label
  )

  # Build a short name for the function (e.g. "character", "logical"), or default to the part after 'is.' if present.
  # For instance, if fn_name is "is.character" -> "character"
  # else if fn_name is "is.data.frame" -> "data.frame"
  # else "something" if we can't parse it out
  if (grepl("^is\\.", fn_name)) {
    expected_type <- sub("^is\\.", "", fn_name)
  } else {
    expected_type <- fn_name
  }

  glue::glue(
    "'{arg_label}' should be {expected_type}. Got: {arg_value}."
  )
}

#' Validate Conditions
#'
#' This function validates input conditions. It checks that each argument is
#' a single logical value (TRUE or FALSE).
#' If any condition is invalid or does not hold, the function aborts with an
#' appropriate error message including the failed condition, a backtrace,
#' and a more verbose explanation when the condition is of the form
#' e.g. \code{is.character(x)}.
#'
#' @param ... Any number of logical conditions.
#'
#' @return `NULL`. The function is called for its side effects.
#' @export
#'
#' @examples
#' validate(1 == 1, 2 == 2, is.function(print))
#'
#' # Will abort with a descriptive error and backtrace:
#' # validate(is.character(1))
#' # validate(TRUE, FALSE)
validate <- function(...) {
  withr::with_options(
    list(rlang_backtrace_on_error = "full", error = rlang::entrace),
    {
      conditions <- list(...)
      conditions_expr <- as.list(substitute(list(...)))[-1]

      for (i in seq_along(conditions)) {
        cond <- conditions[[i]]
        cond_expr <- conditions_expr[[i]]

        if (!is.logical(cond) || length(cond) != 1) {
          cli::cli_abort(
            message = glue::glue(
              "Condition must be a single logical value (TRUE or FALSE): {deparse(cond_expr)}"
            ),
            .subclass = "validation_error"
          )
        }

        if (!isTRUE(cond)) {
          parsed_message <- parse_condition(cond_expr, env = parent.frame())
          cli::cli_abort(
            message = parsed_message,
            .subclass = "validation_error"
          )
        }
      }
      invisible(NULL) # All conditions passed
    }
  )
}

#' Check that a data frame contains specific columns
#'
#' @param df *\[data.frame\]* The data frame to check
#' @param columns *\[vector, character\]* A set of columns to check
#' @return `NULL`. Checks that the columns exist in the data frame
#' @example
#' validate_columns(df, c("effect", "se"))
#' @export
validate_columns <- function(df, columns) {
  if (!is.data.frame(df)) {
    cli::cli_abort("'df' must be a data frame.")
  }
  if (!is.character(columns)) {
    cli::cli_abort("'columns' must be a character vector")
  }

  if (!all(columns %in% colnames(df))) {
    cli::cli_abort(glue::glue_collapse("Invalid column names:", glue::glue_collapse(colnames(df), sep = ", ")), "Expected to contain:", paste(columns, sep = ", "))
  }
}

#' @title Validate value type
#' @description A helper function that checks if a value matches the expected type. Returns TRUE if it does, and FALSE otherwise.
#' @param value [any] The value to check
#' @param expected_type *\[character\]* The expected type.
#'  *\[logical\]* TRUE if the value matches the expected type, FALSE otherwise.
#' @export
validate_value_type <- function(value, expected_type) {
  switch(expected_type,
    character = is.character(value),
    numeric   = is.numeric(value),
    integer   = is.numeric(value) && (floor(value) == value),
    logical   = is.logical(value),
    # Otherwise, we default to TRUE (or you can flag as unvalidated)
    TRUE
  )
}

#' @title Assert Conditions
#' @description This function asserts that a condition is TRUE. If the condition is FALSE,
#' the function aborts with an appropriate error message including the failed
#' condition and a backtrace.
#' @param condition_to_validate *\[logical\]* The condition to validate.
#' @param msg [character(1), optional] The error message to display if the condition is FALSE.
#' @return `NULL`. The function is called for its side effects.
#' @export
assert <- function(condition_to_validate, msg = NULL) {
  if (is.null(msg)) {
    msg <- paste("Assertion failed:", deparse(substitute(condition_to_validate)))
  }
  if (!condition_to_validate) {
    cli::cli_abort(
      message = msg,
      .subclass = "assertion_error"
    )
  }
}

#' Check that an options template file exists under a given path. If it does not, raise an error.
#' @param path *\[character\]* The path under which the file should exist.
#' @export
assert_options_template_exists <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort(glue::glue("The options template file does not exist under the following path: '{path}'."))
  }
  if (!grepl(".yaml$|.yml$", path)) {
    cli::cli_abort(glue::glue("The path to the template file is invalid. Reason: Missing the .yaml suffix. Got: {path}."))
  }
  return(invisible(NULL))
}

#' Check if x is a vector and either empty or all characters
#'
#' @param x [any] The object to check
#' @param throw_error *\[logical\]* Whether to throw an error if the check fails
#' @return *\[boolean\]* A boolean indicating whether or not x is a character vector or empty
#' @export
is_char_vector_or_empty <- function(x, throw_error = FALSE) {
  is_empty <- rlang::is_empty(x)
  if (throw_error && !is_empty) {
    cli::cli_abort("The object is not a character vector or empty")
  }
  return(is_empty)
}

#' @title Check if an option path is valid
#' @description Check if an option path is valid
#' @param opt_path *\[character\]* The option path to check.
#' @return *\[logical\]* Whether the option path is valid.
#' @examples
#' validate_opt_path("data.colnames") # passes
#' validate_opt_path("data.colnames.obs_id") # passes
#' validate_opt_path(c("data.colnames", "data.colnames.obs_id")) # fails
#' validate_opt_path(NULL) # passes
#' @export
validate_opt_path <- function(opt_path) {
  if (is.null(opt_path)) {
    return(invisible(NULL))
  }
  if (!is.character(opt_path)) {
    cli::cli_abort("The option path must be a character string.")
  }
  if (length(opt_path) != 1) {
    cli::cli_abort("The option path must be a single character string.")
  }
  if (!grepl("^[[:alnum:]_]+(\\.[[:alnum:]_]+)*$", opt_path)) {
    cli::cli_abort("The option path must be a single word or dot-separated path (e.g. 'data' or 'data.colnames')")
  }
  return(invisible(NULL))
}
