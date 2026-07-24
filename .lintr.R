# nolint start: indentation_linter, undesirable_function_linter, unused_declared_object.

custom_linters_env <- new.env()

# Locate the repository root without the here package, which is not a
# package dependency and may not be installed in the linting session.
repo_root <- local({
  path <- getwd()
  while (!file.exists(file.path(path, "DESCRIPTION")) && dirname(path) != path) {
    path <- dirname(path)
  }
  path
})

# The custom linters live under scripts/ so that the installed package does
# not carry lintr as a runtime dependency.
source(
  file.path(repo_root, "scripts", "linters.R"),
  local = custom_linters_env,
  chdir = TRUE
)

# Resolve box modules against the checkout being linted, overriding any
# box.path inherited from .Rprofile that may point at a different checkout
# (e.g. when linting inside a worktree). Required by box_usage_linter.
options(box.path = file.path(repo_root, "inst"))

linters <- c(
  lintr::linters_with_defaults(
    # https://lintr.r-lib.org/reference/index.html#individual-linters
    #
    # All default lintr linters (resolved from the lintr namespace)
    defaults = default_linters,
    # Set indentation to 2 spaces
    indentation_linter = custom_linters_env$indentation_guard_clause_linter(indent = 2),
    # Check that all commas are followed by spaces, but do not have spaces before them.
    commas_linter = lintr::commas_linter(allow_trailing = FALSE),
    # Check that all comments are preceded by a space
    object_name_linter = lintr::object_name_linter(
      styles = c("snake_case", "SNAKE_CASE", "dotted.case")
    ),
    object_length_linter = lintr::object_length_linter(length = 40),
    # Disable the default lintr object usage - incompatible with box module system
    object_usage_linter = NULL,
    # No line length limit enforced
    line_length_linter = lintr::line_length_linter(NULL),
    # Disable commented code linter
    commented_code_linter = NULL,
    # Turn off linting for several functions otherwise flagged as undesirable
    undesirable_function_linter = lintr::undesirable_function_linter(
      fun = lintr::modify_defaults(
        "defaults" = lintr::default_undesirable_functions,
        "options" = NULL, # Remove down the line
        "ifelse" = "use 'res <- if (x) expr1 else expr2'",
        # Console print methods
        "print" = "use cli::cli_inform()",
        "cat" = "use cli::cli_inform()",
        # Base messaging
        "message" = "use cli::cli_inform()",
        "warning" = "use cli::cli_warn()",
        "stop" = "use cli::cli_abort()",
        # rlang messaging
        "inform" = "use cli::cli_inform()",
        "warn" = "use cli::cli_warn()",
        "abort" = "use cli::cli_abort()"
      )
    ),
    # Check for missing packages and symbols in namespace calls
    namespace_linter = lintr::namespace_linter()
  ),
  # Custom linters
  # Flag literal '<U+XXXX>' escape text left behind by C-locale rewrites
  unicode_escape_linter = custom_linters_env$unicode_escape_linter(),
  # Flag calls to functions neither defined in the file nor imported via
  # box::use(); replaces object_usage_linter for the box module system
  box_usage_linter = box.linters::box_usage_linter()
)

exclusions <- list(
  "local/",
  "scripts/",
  ".githooks/",
  # Test files import packages and modules in a single box::use() call, a
  # style box_usage_linter cannot parse (it drops the package attachments and
  # flags every use). Missing imports in tests fail at runtime anyway.
  "tests/" = list(box_usage_linter = Inf)
)

# Clean up helper objects so lintr does not report them as unused settings
rm(list = ls(custom_linters_env), envir = custom_linters_env)
rm(custom_linters_env, repo_root)
gc() # Clean up memory

# nolint end.
