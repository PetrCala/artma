#' @title Leveled CLI logging
#' @description Wrappers around `cli::cli_alert_*()`/`cli::cli_inform()` that
#'   check `get_verbosity()` internally, so call sites stop repeating
#'   `if (get_verbosity() >= n) cli::cli_...(...)` (and, worse, re-deriving `n`
#'   via a hardcoded `getOption("artma.verbose", <default>)` that drifts from
#'   `CONST$DEFAULT_VERBOSITY`). One wrapper per documented verbosity level:
#'   1 errors, 2 warnings, 3 info (the default), 4 debug. Each forwards `...`
#'   and evaluates glue interpolation in the caller's environment, so a call
#'   such as `log_info("Read {.val {n}} rows")` behaves exactly like the
#'   `cli::cli_alert_*()` call it replaces.
#'
#'   The `is_*_enabled()` predicates expose the same thresholds for call
#'   sites that gate something other than a single message (e.g. whether to
#'   render a progress bar) on a verbosity level, without comparing verbosity
#'   numbers themselves outside this module.
box::use(artma / libs / core / utils[get_verbosity])

#' @title Check whether error-level logging is enabled
#' @description `TRUE` when `get_verbosity() >= 1`.
#' @return *\[logical\]*
#' @export
is_error_enabled <- function() get_verbosity() >= 1

#' @title Check whether warn-level logging is enabled
#' @description `TRUE` when `get_verbosity() >= 2`.
#' @return *\[logical\]*
#' @export
is_warn_enabled <- function() get_verbosity() >= 2

#' @title Check whether info-level logging is enabled
#' @description `TRUE` when `get_verbosity() >= 3` (the default verbosity).
#' @return *\[logical\]*
#' @export
is_info_enabled <- function() get_verbosity() >= 3

#' @title Check whether debug-level logging is enabled
#' @description `TRUE` when `get_verbosity() >= 4`.
#' @return *\[logical\]*
#' @export
is_debug_enabled <- function() get_verbosity() >= 4

#' @title Log an error-level message
#' @description Emits via `cli::cli_alert_danger()` when
#'   `get_verbosity() >= 1`. This is for non-fatal error reporting (the
#'   analysis continues); use `cli::cli_abort()` directly to actually stop
#'   execution, since aborting is never gated on verbosity.
#' @param text *\[character\]* Message, following the same glue/cli syntax as
#'   `cli::cli_alert_danger()`.
#' @param ... Forwarded to `cli::cli_alert_danger()`.
#' @param .envir *\[environment\]* Environment glue interpolation resolves
#'   `{}` expressions in. Defaults to the caller's environment.
#' @return `NULL` (invisible).
#' @export
log_error <- function(text, ..., .envir = parent.frame()) {
  if (is_error_enabled()) {
    cli::cli_alert_danger(text, ..., .envir = .envir)
  }
  invisible(NULL)
}

#' @title Log a warning-level message
#' @description Emits via `cli::cli_alert_warning()` when
#'   `get_verbosity() >= 2`.
#' @param text *\[character\]* Message, following the same glue/cli syntax as
#'   `cli::cli_alert_warning()`.
#' @param ... Forwarded to `cli::cli_alert_warning()`.
#' @param .envir *\[environment\]* Environment glue interpolation resolves
#'   `{}` expressions in. Defaults to the caller's environment.
#' @return `NULL` (invisible).
#' @export
log_warn <- function(text, ..., .envir = parent.frame()) {
  if (is_warn_enabled()) {
    cli::cli_alert_warning(text, ..., .envir = .envir)
  }
  invisible(NULL)
}

#' @title Log an info-level message
#' @description Emits via `cli::cli_alert_info()` when `get_verbosity() >= 3`
#'   (the default verbosity).
#' @param text *\[character\]* Message, following the same glue/cli syntax as
#'   `cli::cli_alert_info()`.
#' @param ... Forwarded to `cli::cli_alert_info()`.
#' @param .envir *\[environment\]* Environment glue interpolation resolves
#'   `{}` expressions in. Defaults to the caller's environment.
#' @return `NULL` (invisible).
#' @export
log_info <- function(text, ..., .envir = parent.frame()) {
  if (is_info_enabled()) {
    cli::cli_alert_info(text, ..., .envir = .envir)
  }
  invisible(NULL)
}

#' @title Log a debug-level message
#' @description Emits via `cli::cli_inform()` when `get_verbosity() >= 4`.
#' @param text *\[character\]* Message, following the same glue/cli syntax as
#'   `cli::cli_inform()`.
#' @param ... Forwarded to `cli::cli_inform()`.
#' @param .envir *\[environment\]* Environment glue interpolation resolves
#'   `{}` expressions in. Defaults to the caller's environment.
#' @return `NULL` (invisible).
#' @export
log_debug <- function(text, ..., .envir = parent.frame()) {
  if (is_debug_enabled()) {
    cli::cli_inform(text, ..., .envir = .envir)
  }
  invisible(NULL)
}

box::export(
  log_error,
  log_warn,
  log_info,
  log_debug,
  is_error_enabled,
  is_warn_enabled,
  is_info_enabled,
  is_debug_enabled
)
