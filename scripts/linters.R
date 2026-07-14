# Custom lintr linters for this repository.
#
# This file is development tooling, not part of the installed package. It is
# sourced by .lintr.R (which lintr reads whenever it looks up settings for
# this repository) and by the linter unit tests under tests/testthat/.
# It requires the lintr package to be installed at source time.

# Allow guard clause `if` statements without braces.
#
# Wraps lintr::indentation_linter() to avoid emitting indentation warnings for guard-clause
# style `if` statements where a single, indented expression immediately follows the condition
# on the next line. This keeps the rest of the indentation behavior intact while permitting the
# brace-less guard clause convention adopted in the codebase.
#
# Args:
#   indent: Integer number of spaces to use for indentation checks.
#   ...: Additional arguments forwarded to lintr::indentation_linter().
indentation_guard_clause_linter <- function(indent = 2L, ...) {
  base_linter <- lintr::indentation_linter(indent = indent, ...)

  lintr::Linter(
    function(source_expression) {
      lints <- base_linter(source_expression)

      if (length(lints) == 0L) {
        return(list())
      }

      file_lines <- source_expression$file_lines

      should_keep <- vapply(
        lints,
        function(lint) {
          line_number <- lint$line_number

          if (is.null(line_number) || is.na(line_number) || line_number <= 1L) {
            return(TRUE)
          }

          prev_line <- file_lines[[as.character(line_number - 1L)]]

          if (is.null(prev_line)) {
            return(TRUE)
          }

          prev_trim <- trimws(prev_line)

          if (!grepl("^(if|else if)\\b", prev_trim) || grepl("\\{\\s*$", prev_trim)) {
            return(TRUE)
          }

          current_line <- file_lines[[as.character(line_number)]]

          if (is.null(current_line) || !grepl("^\\s", current_line) || !nzchar(trimws(current_line))) {
            return(TRUE)
          }

          prev_indent <- attr(regexpr("^\\s*", prev_line), "match.length")
          current_indent <- attr(regexpr("^\\s*", current_line), "match.length")

          if (is.na(prev_indent) || is.na(current_indent)) {
            return(TRUE)
          }

          if (!isTRUE(current_indent == prev_indent + indent)) {
            return(TRUE)
          }

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

# Disallow literal `<U+XXXX>` escape text.
#
# Flags literal `<U+XXXX>` sequences in source files. These appear when a file containing
# real UTF-8 characters is rewritten under the C locale (e.g. by styler or an R session
# without a UTF-8 locale), mangling the characters into escape text. Restore the intended
# character or use an ASCII equivalent instead.
unicode_escape_linter <- function() {
  lintr::Linter(
    function(source_expression) {
      file_lines <- source_expression$file_lines
      pattern <- "<U\\+[0-9A-Fa-f]{4,6}>"

      lints <- list()

      for (idx in seq_along(file_lines)) {
        line <- file_lines[[idx]]

        if (is.na(line)) {
          next
        }

        match_start <- regexpr(pattern, line)

        if (is.na(match_start) || match_start == -1L) {
          next
        }

        line_number <- names(file_lines)[[idx]]
        line_number <- if (is.null(line_number) || !nzchar(line_number)) idx else as.integer(line_number)

        lints[[length(lints) + 1L]] <- lintr::Lint(
          filename = source_expression$filename,
          line_number = line_number,
          column_number = as.integer(match_start),
          type = "error",
          message = paste(
            "Literal '<U+XXXX>' escape text found, likely from a rewrite under the C locale.",
            "Restore the intended character or replace it with an ASCII equivalent."
          ),
          line = line
        )
      }

      lints
    },
    name = "unicode_escape_linter",
    linter_level = "file"
  )
}

# Disallow `dir.create()` function calls.
#
# This linter flags any usage of the dir.create() function, which is not permitted in the codebase.
# Using dir.create() can lead to unintended side effects such as creating directories during script execution.
# Instead, consider alternative approaches for managing directories.
dir_create_linter <- lintr::make_linter_from_xpath(
  xpath = "expr[SYMBOL_FUNCTION_CALL[text() = 'dir.create']]",
  lint_message = "Usage of dir.create() is not allowed. Use fs::dir_create() instead.",
  type = "error"
)
