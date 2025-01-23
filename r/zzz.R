#' @title Ensure valid box path
#'
#' @usage Should be called at the top of every exported function
#' @description
#' Ensure that box imports throughout the projects work. This is done by adding the package path to the box path option if it is not already there.
#'
#' @keywords internal
ensure_valid_boxpath <- function() {
  current_box_path <- getOption("box.path", character(0))
  pkg_box_path <- find.package("artma")
  dev_box_path <- paste(pkg_box_path, "inst", sep = "/") # For local development

  if (!any(grepl("artma$", current_box_path))) {
    # Make the package available to the box options(
    options(box.path = c(current_box_path, pkg_box_path, dev_box_path))
  }
}

#' @title Static Setup
#'
#' @description
#' A function to be called at the beginning of each static setup function to ensure crucial fucntionality, such as imports, logging, etc., all work as expected.
#' @keywords internal
static_setup <- function() {
  ensure_valid_boxpath()
}

#' @title Runtime Setup
#'
#' @description
#' A function to be called at the beginning of each exported runtime function to ensure crucial fucntionality, such as imports, logging, etc., all work as expected.
#' @keywords internal
runtime_setup <- function() {
  static_setup()
  box::use(
    artma / libs / logs / index[setup_logging]
  )
  # Here, there should possibly be validations that options have been set up, and the runtime functions are ready

  setup_logging()
}
