# nolint start: unused_declared_object_linter, box_usage_linter.

#' @title Runtime Setup
#' @description
#' A function user as a wrapper for runtime functions invocation to ensure crucial fucntionality, such as imports, logging, etc., all work as expected.
#'
#' @param FUN [function] The function to be called after the setup.
#' @param options_file_name *\[character\]* Name of the options file to use, including the suffix.
#' @param options_dir *\[character, optional\]* Path to the directory that contains user options. Defaults to the directory specified in PATHS.
#' @keywords internal
runtime_setup <- function(
    FUN,
    options_file_name = NULL,
    options_dir = NULL) {
  # box::use(logs = artma / libs / logs / index)
  tryCatch(
    {
      eval(parse(text = "box::use(logs = artma / libs / logs / index)"))
    },
    error = function(e) {
      message("An error occurred: ", e$message)
      message("Traceback:")
      traceback()
    }
  )

  runtime_options <- options.load(options_file_name = options_file_name, options_dir = options_dir)
  withr::local_options(runtime_options)

  logs$setup_logging()
  FUN()
}

# nolint end: unused_declared_object_linter, box_usage_linter.
