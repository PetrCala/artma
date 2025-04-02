# nolint start: unused_declared_object_linter, box_usage_linter.

#' @title Ensure valid box path
#' @description
#' Ensure that box imports throughout the projects work. This is done by adding the package path to the box path option if it is not already there.
#' @note Should be called at the top of every exported function
#' @keywords internal
ensure_valid_boxpath <- function() {
  current_box_path <- getOption("box.path", character(0))
  pkg_box_path <- find.package("artma")
  dev_box_path <- file.path(pkg_box_path, "inst") # For local development

  if (!any(grepl("artma$", current_box_path))) { # should end with 'artma'
    # Make the package available to the box options
    options(box.path = unique(c(current_box_path, pkg_box_path, dev_box_path)))
  }
}

#' @title Static Setup
#' @description
#' A function to be called at the beginning of each static setup function to ensure crucial fucntionality, such as imports, logging, etc., all work as expected.
#' @keywords internal
static_setup <- function() {
  ensure_valid_boxpath()
}

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
  static_setup()

  box::use(
    artma[options.load],
    logs = artma / libs / logs / index
  )

  withr::with_options(
    options.load(options_file_name = options_file_name, options_dir = options_dir),
    {
      logs$setup_logging()
      FUN()
    }
  )
}

# nolint end: unused_declared_object_linter, box_usage_linter.
