# nolint start: unused_declared_object_linter.

#' Allow Guard Clause `if` Statements Without Braces
#'
#' Wraps [lintr::indentation_linter()] to avoid emitting indentation warnings for guard-clause
#' style `if` statements where a single, indented expression immediately follows the condition
#' on the next line. This keeps the rest of the indentation behavior intact while permitting the
#' brace-less guard clause convention adopted in the codebase.
#'
#' @param indent Integer number of spaces to use for indentation checks.
#' @param ... Additional arguments forwarded to [lintr::indentation_linter()].
#'
#' @importFrom lintr indentation_linter Linter
#' @keywords internal
indentation_guard_clause_linter <- function(indent = 2L, ...) {
  base_linter <- lintr::indentation_linter(indent = indent, ...)

  lintr::Linter(
    function(source_expression) {
      lints <- base_linter(source_expression)

      if (length(lints) == 0L)
        return(list())

      file_lines <- source_expression$file_lines

      should_keep <- vapply(
        lints,
        function(lint) {
          line_number <- lint$line_number

          if (is.null(line_number) || is.na(line_number) || line_number <= 1L)
            return(TRUE)

          prev_line <- file_lines[[as.character(line_number - 1L)]]

          if (is.null(prev_line))
            return(TRUE)

          prev_trim <- trimws(prev_line)

          if (!grepl("^(if|else if)\\b", prev_trim) || grepl("\\{\\s*$", prev_trim))
            return(TRUE)

          current_line <- file_lines[[as.character(line_number)]]

          if (is.null(current_line) || !grepl("^\\s", current_line) || !nzchar(trimws(current_line)))
            return(TRUE)

          prev_indent <- attr(regexpr("^\\s*", prev_line), "match.length")
          current_indent <- attr(regexpr("^\\s*", current_line), "match.length")

          if (is.na(prev_indent) || is.na(current_indent))
            return(TRUE)

          if (!isTRUE(current_indent == prev_indent + indent))
            return(TRUE)

          FALSE
        },
        logical(1)
      )

      lints[should_keep]
    },
    name = "indentation_guard_clause_linter",
    linter_level = attr(base_linter, "linter_level", exact = TRUE)
  )
}

#' Disallow `dir.create()` Function Calls
#'
#' This linter flags any usage of the [dir.create()] function, which is not permitted in the codebase.
#' Using `dir.create()` can lead to unintended side effects such as creating directories during script execution.
#' Instead, consider alternative approaches for managing directories.
#'
#' @importFrom lintr make_linter_from_xpath
#' @keywords internal
dir_create_linter <- lintr::make_linter_from_xpath(
  xpath = "expr[SYMBOL_FUNCTION_CALL[text() = 'dir.create']]",
  lint_message = "Usage of dir.create() is not allowed. Use fs::dir_create() instead.",
  type = "error"
)


# nolint end: unused_declared_object_linter.
