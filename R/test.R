# nolint start: namespace_linter.

#' @keywords internal
test <- function() { # nolint: unused_declared_object_linter.
  # A development function for invoking various chunks of code

  # Test the 'run' method
  # option_files <- c("default.yaml", "test.yaml", "test2.yaml")
  # for (file in option_files) {
  #   artma::run(methods = "variable_summary_stats", options_file_name = file)
  # }

  # Test various 'options' module related methods
  options.validate("test.yaml")
}

# nolint end: namespace_linter.
