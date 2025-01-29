source(file.path("scripts", "src", "linters.R"), local = TRUE) # nolint

linters <- lintr::all_linters(
    # https://lintr.r-lib.org/reference/index.html#individual-linters # nolint
    #
    # Set indentation to 8 spaces
    indentation_linter = lintr::indentation_linter(2L),
    # Check that all commas are followed by spaces, but do not have spaces before them.
    commas_linter = lintr::commas_linter(allow_trailing = FALSE),
    # Check that all comments are preceded by a space
    object_name_linter = lintr::object_name_linter(styles = c("snake_case", "SNAKE_CASE", "dotted.case")),
    # Disable the object usage linter
    object_usage_linter = NULL,
    # Disable line length limiters
    line_length_linter = NULL,
    # Disable assignment linter if you use assignment arrows other than `<-`
    assignment_linter = NULL,
    # Disable commented code linter
    commented_code_linter = NULL,
    # Disable cyclocompexity linter
    cyclocomp_linter = NULL,
    pipe_call_linter = pipe_call_linter(),
    dir_create_linter = dir_create_linter()
)
### Custom linters
# dir_create_linter)
# Disable the use of 'dir.create()' in favor of 'fs::dir_create()'
# dir_create_linter = readRDS("custom_linters.rds")$dir_create_linter)
exclusions <- list(
    "tests/" # nolint
)
