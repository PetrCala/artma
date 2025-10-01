#' @title List methods
#' @description Print all runtime methods supported by artma into the console.
#' @return `NULL` Prints the available methods into the console.
#' @export
methods.list <- function() {
  box::use(
    artma / const[CONST],
    artma / libs / utils[get_verbosity],
    artma / modules / runtime_methods[get_runtime_method_modules]
  )

  RUNTIME_METHOD_MODULES <- get_runtime_method_modules() # nolint: unused_declared_object_linter.

  if (get_verbosity() >= 2) {
    cli::cli_h1("{.file {CONST$PACKAGE_NAME}} ({packageVersion(CONST$PACKAGE_NAME)}) runtime methods:")
    cli::cli_ul(names(RUNTIME_METHOD_MODULES))
  }
}
