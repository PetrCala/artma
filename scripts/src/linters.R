#!/usr/bin/env Rscript

# #' @evalRd rd_tags("dir_create_linter")
# #' @seealso [linters] for a complete list of linters available in lintr.
# #' @export
# dir_create_linter <- function() {
#   lintr::Linter(function(source_file) {
#     # Identify all SYMBOL_FUNCTION_CALL tokens in the parse data
#     call_positions <- lintr::ids_with_token(source_file, "SYMBOL_FUNCTION_CALL")
#     # The parsed content for each token
#     parsed <- source_file$parsed_content

#     # We only care about calls whose text is "dir.create"
#     is_dir_create <- parsed$text[call_positions] == "dir.create"
#     dir_create_positions <- call_positions[is_dir_create]

#     # For each position, return a Lint object if it is *not* excluded
#     # and we haven't suppressed it with `# nolint` or a specialized directive.
#     lapply(dir_create_positions, function(pos) {
#       # If the line is excluded via `# nolint` or a more specific `# nolint: dir_create_linter`
#       # then lintr::is_linted() will return FALSE, so we skip creating a lint.
#       if (!lintr::is_linted(
#         source_file = source_file, line_number = parsed$line1[pos],
#         linter_name = "dir_create_linter"
#       )) {
#         return(NULL)
#       }

#       lintr::Lint(
#         filename = source_file$filename,
#         line_number = parsed$line1[pos],
#         column_number = parsed$col1[pos],
#         type = "warning", # or "error"
#         message = "Usage of dir.create() is not allowed (unless explicitly overridden).",
#         linter = "dir_create_linter"
#       )
#     }) |> unlist(recursive = FALSE)
#   })
# }


#' Box import linter
#'
#' Force box imports across the project
#'  - Use 'box::' for importing modules
#' @export
# box_import_linter <- function() {
#   lintr::Linter(function(source_file) {
#     lapply(lintr::ids_with_token(source_file, "SYMBOL_PACKAGE"), function(id) {
#       token <- lintr::with_id(source_file, id)
#       if (token$text != "box::") {
#         lintr::Lint(
#           filename = source_file$filename,
#           line_number = token$line1,
#           column_number = token$col1,
#           type = "style",
#           message = "Use 'box::' for importing modules",
#           line = source_file$lines[token$line1]
#         )
#       }
#     })
#   })
# }


# #' @evalRd rd_tags("dir_create_linter")
# #' @seealso [linters] for a complete list of linters available in lintr.
# #' @export
# dir_create_linter <- function() {
#   lintr::Linter(linter_level = "file", function(source_expression) {
#     # Identify all SYMBOL_FUNCTION_CALL tokens in the parse data
#     call_positions <- lintr::ids_with_token(source_file, "SYMBOL_FUNCTION_CALL")
#     # The parsed content for each token
#     parsed <- source_file$parsed_content

#     # We only care about calls whose text is "dir.create"
#     is_dir_create <- parsed$text[call_positions] == "dir.create"
#     dir_create_positions <- call_positions[is_dir_create]

#     # For each position, return a Lint object if it is *not* excluded
#     # and we haven't suppressed it with `# nolint` or a specialized directive.
#     lapply(dir_create_positions, function(pos) {
#       # If the line is excluded via `# nolint` or a more specific `# nolint: dir_create_linter`
#       # then lintr::is_linted() will return FALSE, so we skip creating a lint.
#       # if (!lintr::is_linted(
#       #   source_file = source_file, line_number = parsed$line1[pos],
#       #   linter_name = "dir_create_linter"
#       # )) {
#       #   return(NULL)
#       # }

#       lintr::Lint(
#         filename = source_file$filename,
#         line_number = parsed$line1[pos],
#         column_number = parsed$col1[pos],
#         type = "warning",
#         message = "Usage of dir.create() is not allowed (unless explicitly overridden).",
#         linter = "dir_create_linter"
#       )
#     }) |> unlist(recursive = FALSE)
#   })
# }


dir_create_linter <- lintr::make_linter_from_xpath(
  xpath = "expr[SYMBOL_FUNCTION_CALL[text() = 'dir.create']]",
  lint_message = "Usage of dir.create() is not allowed."
  # function_names = "dir.create",
)
