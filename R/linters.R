# nolint start: unused_declared_object_linter.

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
