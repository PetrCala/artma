#!/usr/bin/env Rscript

#' Box import linter
#'
#' Force box imports across the project
#'  - Use 'box::' for importing modules
box_import_linter <- function(source_file) {
  lapply(ids_with_token(source_file, "SYMBOL_PACKAGE"), function(id) {
    token <- with_id(source_file, id)
    if (token$text != "box::") {
      Lint(
        filename = source_file$filename,
        line_number = token$line1,
        column_number = token$col1,
        type = "style",
        message = "Use 'box::' for importing modules",
        line = source_file$lines[token$line1]
      )
    }
  })
}

box_object_usage_linter <- lintr::Linter(function(source_file) {
  linter <- lintr::object_usage_linter()
  lints <- linter$lint(source_file)

  # Filter out lints for objects imported via box::use
  box_imports <- source_file$parsed_content %>%
    dplyr::filter(token == "SYMBOL_PACKAGE" & text == "box") %>%
    dplyr::pull(line1)

  filtered_lints <- lints %>%
    dplyr::filter(!line %in% box_imports)

  return(filtered_lints)
})

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
