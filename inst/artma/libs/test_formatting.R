#' @title Test formatting helpers
#' @description Formatting helpers shared across testing modules.
NULL

significance_mark <- function(p_value) {
  if (!is.finite(p_value)) {
    return("")
  }
  if (p_value <= 0.01) {
    return("***")
  }
  if (p_value <= 0.05) {
    return("**")
  }
  if (p_value <= 0.1) {
    return("*")
  }
  ""
}

format_number <- function(x, digits) {
  if (!length(x)) {
    return(character(0))
  }

  finite <- is.finite(x)
  finite[is.na(finite)] <- FALSE

  formatted <- rep(NA_character_, length(x))
  if (any(finite)) {
    formatted[finite] <- formatC(round(x[finite], digits), format = "f", digits = digits)
  }

  if (!is.null(names(x))) {
    names(formatted) <- names(x)
  }

  formatted
}

format_se <- function(se, digits) {
  if (!length(se)) {
    return(character(0))
  }

  finite <- is.finite(se)
  finite[is.na(finite)] <- FALSE

  formatted <- rep(NA_character_, length(se))
  if (any(finite)) {
    formatted[finite] <- paste0("(", format_number(se[finite], digits), ")")
  }

  if (!is.null(names(se))) {
    names(formatted) <- names(se)
  }

  formatted
}

format_ci <- function(lower, upper, digits) {
  if (!length(lower) || !length(upper)) {
    return(character(0))
  }

  finite <- is.finite(lower) & is.finite(upper)
  finite[is.na(finite)] <- FALSE

  formatted <- rep(NA_character_, length(lower))
  if (any(finite)) {
    lower_fmt <- format_number(lower[finite], digits)
    upper_fmt <- format_number(upper[finite], digits)
    formatted[finite] <- paste0("[", lower_fmt, ", ", upper_fmt, "]")
  }

  if (!is.null(names(lower))) {
    names(formatted) <- names(lower)
  }

  formatted
}

box::export(
  significance_mark,
  format_number,
  format_se,
  format_ci
)
