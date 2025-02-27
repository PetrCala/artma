#' @title List methods
#' @description Print all runtime methods supported by ARTMA into the console.
#' @export
methods.list <- function() {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level

  box::use(artma / const[CONST])

  cli::cli_text(paste("As of the current version,", CONST$PACKAGE_NAME, "supports the following runtime methods:"))
  cli::cli_ul(CONST$SUPPORTED_METHODS)
}