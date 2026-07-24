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

#' @title Format an estimate together with its significance mark
#' @description Like `format_number()`, but appends a significance mark and
#'   keeps non-finite estimates as `NA_character_`. A plain
#'   `paste0(format_number(x), mark)` turns an `NA` estimate into the literal
#'   string `"NA"`, which then slips past every downstream `is.na()` guard.
#' @param x *[numeric]* Estimates to format.
#' @param digits *[integer]* Number of decimals.
#' @param marks *[character]* Significance marks, recycled against `x`.
#' @return *[character]* Formatted estimates, `NA_character_` where not finite.
format_estimate <- function(x, digits, marks = "") {
  formatted <- format_number(x, digits)
  if (!length(formatted)) {
    return(formatted)
  }

  finite <- !is.na(formatted)
  if (any(finite)) {
    formatted[finite] <- paste0(formatted[finite], rep_len(marks, length(formatted))[finite])
  }

  formatted
}

#' @title Format an estimate whose marks are derived from a raw p-value
#' @description
#' Companion to [format_estimate()] for callers that hold a raw p-value and an
#' "append marks or not" flag rather than a precomputed marks vector.
#' Note: unlike [format_estimate()], a non-finite `x` here ends up as the
#' literal string `"NA"` rather than `NA_character_`, because the marks are
#' pasted on before the finiteness check. This mirrors the historical
#' behaviour of the callers this consolidates and is preserved intentionally.
#' @param x *[numeric]* Estimates to format.
#' @param p_value *[numeric]* Two-sided p-values used to derive marks.
#' @param digits *[integer]* Number of decimals.
#' @param add_marks *[logical]* Whether to append significance marks.
#' @return *[character]* Formatted estimates.
format_estimate_with_pvalue <- function(x, p_value, digits, add_marks) {
  formatted <- format_number(x, digits)
  if (!length(formatted)) {
    return(character(0))
  }
  marks <- if (isTRUE(add_marks)) significance_mark(p_value) else ""
  formatted <- paste0(formatted, marks)
  formatted[is.na(formatted)] <- ""
  formatted
}

#' @title Format a standard error, blanking non-finite values
#' @description Like [format_se()], but renders non-finite standard errors as
#'   an empty string instead of `NA_character_`.
#' @param se *[numeric]* Standard errors to format.
#' @param digits *[integer]* Number of decimals.
#' @return *[character]* Formatted, parenthesised standard errors.
format_standard_error <- function(se, digits) {
  formatted <- format_se(se, digits)
  formatted[is.na(formatted)] <- ""
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
  invisible(lines)
}

#' Pick the cli style function for a verdict tone
#'
#' @param tone *\[character\]* One of `"good"`, `"bad"`, or anything else for
#'   unstyled output.
#' @return *\[function\]* A string-to-string styling function.
#' @keywords internal
tone_style <- function(tone) {
  switch(tone %||% "",
    good = cli::col_green,
    bad = cli::col_red,
    identity
  )
}

#' Print a sectioned key/value summary table through cli
#'
#' @description
#' Renders a tidy `Section` / `Statistic` / `Value` (/ `Note`) frame as
#' left-aligned key/value rows under underlined section headings. Unlike
#' `print.data.frame`, which right-aligns character columns and so destroys any
#' label hierarchy, this keeps labels flush left and right-aligns only the value
#' column, so numbers line up and sub-rows stay readable.
#'
#' Verdict notes are colourised from a `tone` attribute on the frame (a
#' character vector, one entry per row, of `"good"`, `"bad"`, or `""`). The
#' attribute is deliberately not a column, so the exported CSV stays free of
#' presentation-only data.
#'
#' Falls back to [print_summary_table()] when the expected columns are absent.
#'
#' @param summary *\[data.frame\]* The summary table to print.
#' @param section_col *\[character\]* Name of the section column.
#' @param label_col *\[character\]* Name of the label column.
#' @param value_col *\[character\]* Name of the value column.
#' @param note_col *\[character\]* Name of the (optional) verdict column.
#' @param indent *\[character\]* Prefix placed before every non-heading row.
#' @keywords internal
print_sectioned_table <- function(summary,
                                  section_col = "Section",
                                  label_col = "Statistic",
                                  value_col = "Value",
                                  note_col = "Note",
                                  indent = "  ") {
  if (!is.data.frame(summary) || nrow(summary) == 0L) {
    return(invisible(character()))
  }
  if (!all(c(section_col, label_col, value_col) %in% names(summary))) {
    return(print_summary_table(summary))
  }

  as_chr <- function(x, empty = "") {
    out <- as.character(x)
    out[is.na(out)] <- empty
    out
  }

  sections <- as_chr(summary[[section_col]])
  labels <- as_chr(summary[[label_col]])
  values <- as_chr(summary[[value_col]], empty = "NA")
  notes <- if (note_col %in% names(summary)) as_chr(summary[[note_col]]) else rep("", nrow(summary))

  tones <- attr(summary, "tone")
  if (length(tones) != nrow(summary)) {
    tones <- rep("", nrow(summary))
  }
  tones <- as_chr(tones)

  label_width <- max(nchar(labels))
  value_width <- max(nchar(values))

  lines <- character()
  current_section <- NULL
  for (i in seq_along(labels)) {
    if (!identical(sections[[i]], current_section)) {
      current_section <- sections[[i]]
      if (nzchar(current_section)) {
        if (length(lines) > 0L) {
          lines <- c(lines, "")
        }
        lines <- c(lines, current_section, strrep("-", nchar(current_section)))
      }
    }

    line <- sprintf("%s%-*s  %*s", indent, label_width, labels[[i]], value_width, values[[i]])
    if (nzchar(notes[[i]])) {
      line <- paste0(line, "   ", tone_style(tones[[i]])(notes[[i]]))
    }
    lines <- c(lines, line)
  }

  # cli_verbatim() silently drops empty strings, so the blank line separating
  # two sections has to go out through cli_text().
  for (line in lines) {
    if (nzchar(line)) cli::cli_verbatim(line) else cli::cli_text("")
  }

  invisible(lines)
}

#' Print a wrapped paragraph of generated prose through cli
#'
#' @description
#' Wraps and prints sentences via `cli_verbatim` (so cached runs replay the
#' output verbatim, which `cli_text`'s glue interpolation would not survive).
#'
#' @param sentences *\[character\]* Sentences to join into one paragraph.
#' @param width *\[integer\]* Wrap width.
#' @keywords internal
print_paragraph <- function(sentences, width = 88L) {
  sentences <- sentences[nzchar(sentences)]
  if (length(sentences) == 0L) {
    return(invisible(character()))
  }
  lines <- strwrap(paste(sentences, collapse = " "), width = width)
  cli::cli_verbatim(lines)
  invisible(lines)
}

box::export(
  significance_mark,
  format_number,
  format_estimate,
  format_estimate_with_pvalue,
  format_se,
  format_standard_error,
  format_ci,
  print_summary_table,
  print_sectioned_table,
  print_paragraph,
  tone_style
)
