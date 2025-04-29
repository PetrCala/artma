#' @keywords internal
get_runtime_method_modules <- function() {
  box::use(
    artma / paths[PATHS],
    artma / libs / modules[crawl_and_import_modules, validate_runtime_method_modules]
  )

  modules <- crawl_and_import_modules(PATHS$DIR_METHODS)
  validate_runtime_method_modules(modules = modules)

  modules
}

#' @title List methods
#' @description Print all runtime methods supported by artma into the console.
#' @return `NULL` Prints the available methods into the console.
#' @export
methods.list <- function() {
  box::use(
    artma / const[CONST],
    artma / libs / utils[get_verbosity]
  )

  RUNTIME_METHOD_MODULES <- get_runtime_method_modules()

  if (get_verbosity() >= 2) {
    cli::cli_h1("{.file {CONST$PACKAGE_NAME}} ({packageVersion(CONST$PACKAGE_NAME)}) runtime methods:")
    cli::cli_ul(names(RUNTIME_METHOD_MODULES))
  }
}
