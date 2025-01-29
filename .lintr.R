# nolint start: indentation_linter, undesirable_function_linter.

source(file.path("R", "linters.R"), local = TRUE)

linters <- lintr::all_linters(
    # https://lintr.r-lib.org/reference/index.html#individual-linters
    #
    # Set indentation to 8 spaces
    indentation_linter = lintr::indentation_linter(2L),
    # Check that all commas are followed by spaces, but do not have spaces before them.
    commas_linter = lintr::commas_linter(allow_trailing = FALSE),
    # Check that all comments are preceded by a space
    object_name_linter = lintr::object_name_linter(
        styles = c("snake_case", "SNAKE_CASE", "dotted.case")
    ),
    line_length_linter = lintr::line_length_linter(120L),
    # Disable commented code linter
    commented_code_linter = NULL,
    # Disable cyclocompexity linter
    cyclocomp_linter = NULL,
    ### Custom linters
    # Disable the usage of 'dir.create' in favor of 'fs::dir_create'
    dir_create_linter = dir_create_linter()
)

exclusions <- list(
    "tests/"
)

# nolint end: indentation_linter, undesirable_function_linter.
