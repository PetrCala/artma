# nolint start: unused_declared_object_linter.

#' Disallow `dir.create()` Function Calls
#'
#' This linter flags any usage of the [dir.create()] function, which is not permitted in the codebase.
#' Using `dir.create()` can lead to unintended side effects such as creating directories during script execution.
#' Instead, consider alternative approaches for managing directories.
#'
#' @examples
#' \dontrun{
#' # will produce lints
#' lint(
#'   text = 'dir.create("path/to/dir")',
#'   linters = dir_create_linter()
#' )
#'
#' lint(
#'   text = 'if (!dir.exists("path/to/dir")) { dir.create("path/to/dir") }',
#'   linters = dir_create_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'if (!dir.exists("path/to/dir")) { fs::dir_create("path/to/dir") }',
#'   linters = dir_create_linter()
#' )
#' }
#' @seealso [linters] for a complete list of linters available in lintr.
#' @keywords internal
dir_create_linter <- lintr::make_linter_from_xpath(
  xpath = "expr[SYMBOL_FUNCTION_CALL[text() = 'dir.create']]",
  lint_message = "Usage of dir.create() is not allowed. Use fs::dir_create() instead.",
  type = "error"
)


# nolint end: unused_declared_object_linter.
