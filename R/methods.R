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
#' @description Print all runtime methods supported by ARTMA into the console.
#' @return `NULL` Prints the available methods into the console.
#' @export
methods.list <- function() {
  static_setup() # nolint: box_usage_linter.

  box::use(artma / const[CONST])

  RUNTIME_METHOD_MODULES <- get_runtime_method_modules()

  cli::cli_h1("{.file {CONST$PACKAGE_NAME}} ({packageVersion(CONST$PACKAGE_NAME)}) runtime methods:")
  cli::cli_ul(names(RUNTIME_METHOD_MODULES))
}
