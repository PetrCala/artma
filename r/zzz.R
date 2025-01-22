#' @title Ensure valid box path
#'
#' @description
#' Ensure that box imports throughout the projects work. This is done by adding the package path to the box path option if it is not already there.
#'
#' @keywords internal
ensure_valid_boxpath <- function() {
  current_box_path <- getOption("box.path", character(0))
  pkg_box_path <- find.package("artma")

  if (!any(grepl("artma$", current_box_path))) {
    # Make the package available to the box options(
    options(box.path = c(current_box_path, pkg_box_path))
  }
}
