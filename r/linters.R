#!/usr/bin/env Rscript

# #' Box import linter
# #'
# #' Force box imports across the project
# #'  - Use 'box::' for importing modules
# box_import_linter <- function(source_file) {
#   lapply(ids_with_token(source_file, "SYMBOL_PACKAGE"), function(id) {
#     token <- with_id(source_file, id)
#     if (token$text != "box::") {
#       Lint(
#         filename = source_file$filename,
#         line_number = token$line1,
#         column_number = token$col1,
#         type = "style",
#         message = "Use 'box::' for importing modules",
#         line = source_file$lines[token$line1]
#       )
#     }
#   })
# }

# box_object_usage_linter <- lintr::Linter(function(source_file) {
#   linter <- lintr::object_usage_linter()
#   lints <- linter$lint(source_file)

#   # Filter out lints for objects imported via box::use
#   box_imports <- source_file$parsed_content %>%
#     dplyr::filter(token == "SYMBOL_PACKAGE" & text == "box") %>%
#     dplyr::pull(line1)

#   filtered_lints <- lints %>%
#     dplyr::filter(!line %in% box_imports)

#   return(filtered_lints)
# })


#' @evalRd rd_tags("dir_create_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
dir_create_linter <- function() {
  lintr::Linter(function(source_file) {
    # Identify all SYMBOL_FUNCTION_CALL tokens in the parse data
    call_positions <- lintr::ids_with_token(source_file, "SYMBOL_FUNCTION_CALL")
    # The parsed content for each token
    parsed <- source_file$parsed_content

    # We only care about calls whose text is "dir.create"
    is_dir_create <- parsed$text[call_positions] == "dir.create"
    dir_create_positions <- call_positions[is_dir_create]

    # For each position, return a Lint object if it is *not* excluded
    # and we haven't suppressed it with `# nolint` or a specialized directive.
    lapply(dir_create_positions, function(pos) {
      # If the line is excluded via `# nolint` or a more specific `# nolint: dir_create_linter`
      # then lintr::is_linted() will return FALSE, so we skip creating a lint.
      if (!lintr::is_linted(
        source_file = source_file, line_number = parsed$line1[pos],
        linter_name = "dir_create_linter"
      )) {
        return(NULL)
      }

      lintr::Lint(
        filename = source_file$filename,
        line_number = parsed$line1[pos],
        column_number = parsed$col1[pos],
        type = "warning", # or "error"
        message = "Usage of dir.create() is not allowed (unless explicitly overridden).",
        linter = "dir_create_linter"
      )
    }) |> unlist(recursive = FALSE)
  })
}

# Define a custom linter function that uses the custom linters
# custom_linters <- lintr::lint_dir()(
#     # box_object_usage_linter = box_object_usage_linter
# )

# # Function to lint a directory
# lint_directory <- function(dir) {
#     lints <- lintr::lint_dir(path = dir, linters = custom_linters)
#     print(lints)
# }

# # Get directory from command line argument
# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) != 1) {
#     stop("Please provide exactly one directory path as an argument.")
# }
# dir <- args[1]

# # Lint the directory
# lint_directory(dir = dir)

#' Pipe call linter
#'
#' Force explicit calls in magrittr pipes, e.g.,
#' `1:3 %>% sum()` instead of `1:3 %>% sum`.
#'
#' @evalRd rd_tags("pipe_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
pipe_call_linter <- function() {
  xpath <- "//expr[preceding-sibling::SPECIAL[text() = '%>%'] and *[1][self::SYMBOL]]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Use explicit calls in magrittr pipes, i.e., `a %>% foo` should be `a %>% foo()`.",
      type = "warning"
    )
  })
}


#' @export
artma_linters <- lintr::linters_with_defaults(
  pipe_call_linter = pipe_call_linter()
)
