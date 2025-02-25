# nolint start: indentation_linter, undesirable_function_linter, unused_declared_object.

custom_linters_env <- new.env()

source(file.path("R", "linters.R"), local = custom_linters_env, chdir = TRUE)

linters <- c(
    lintr::all_linters(
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
        # Disable the default lintr object usage - replace with box imports
        object_usage_linter = NULL,
        # All lines should be less than 120 characters
        line_length_linter = lintr::line_length_linter(NULL), # 120L, 160L,...
        # Disable commented code linter
        commented_code_linter = NULL,
        # Disable the forbidden access through $ linter
        extraction_operator_linter = NULL,
        # Disable cyclocompexity linter
        cyclocomp_linter = NULL,
        # Disable the string boundary linter - grepl usage, startsWith,...
        string_boundary_linter = NULL,
        # Turn off linting for several functions otherwise flagged as undesirable
        undesirable_function_linter = lintr::undesirable_function_linter(
            fun = lintr::modify_defaults(
                defaults = lintr::default_undesirable_functions,
                options = NULL # We use options extensively through ARTMA options
            )
        )
    ),
    # All default box linters
    box.linters::box_default_linters,
    # Custom linters
    # Disable the usage of 'dir.create' in favor of 'fs::dir_create'
    dir_create_linter = custom_linters_env$dir_create_linter()
)

exclusions <- list(
    "tests/",
    "archive/"
)

# Clean up imported linters
rm(list = ls(custom_linters_env), envir = custom_linters_env)
rm(custom_linters_env)
gc() # Clean up memory

# nolint end.
