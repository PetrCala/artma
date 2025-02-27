#' @keywords internal
test <- function() { # nolint: unused_declared_object_linter.
  # A development function for invoking various chunks of code
  artma::run( # nolint: namespace_linter.
    methods = c("funnel_plot", "bma", "robma"),
    options_file_name = "default.yaml"
  )
}
