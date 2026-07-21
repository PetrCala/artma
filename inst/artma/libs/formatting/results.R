#' @title Test formatting helpers
#' @description Formatting helpers shared across testing modules.
NULL

significance_mark <- function(p_value) {
  if (!length(p_value)) {
    return(character(0))
  }

  finite <- is.finite(p_value)
  finite[is.na(finite)] <- FALSE

  marks <- rep("", length(p_value))
  if (any(finite)) {
    marks[finite & p_value <= 0.1] <- "*"
    marks[finite & p_value <= 0.05] <- "**"
    marks[finite & p_value <= 0.01] <- "***"
  }

  if (!is.null(names(p_value))) {
    names(marks) <- names(p_value)
  }

  marks
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

#' Print a method summary table through cli
#'
#' @description
#' Prints a summary data frame via cli_verbatim (so cached runs replay the
#' output), hiding synthetic row indices when the first column already
#' duplicates the row names.
#'
#' @param summary *\[data.frame\]* The summary table to print
#' @keywords internal
print_summary_table <- function(summary) {
  duplicated_metric <- identical(rownames(summary), summary[[1]])
  if (duplicated_metric) {
    rownames(summary) <- NULL
  }
  lines <- utils::capture.output(
    print(summary, row.names = !duplicated_metric) # nolint: undesirable_function_linter.
  )
  cli::cli_verbatim(lines)
}

box::export(
  significance_mark,
  format_number,
  format_se,
  format_ci,
  print_summary_table
)
