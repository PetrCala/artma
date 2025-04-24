box::use(
  artma / paths[PATHS],
  artma / libs / modules[crawl_and_import_modules]
)

#' Load test modules from a directory matching a pattern.
#'
#' @param dir_path *\[character\]* The directory path to search for modules.
#' @param pattern *\[character\]* A regular expression to filter module file names.
#' @return *\[list\]* A named list of objects imported from the modules.
load_test_modules <- function(dir_path, pattern) {
  modules <- crawl_and_import_modules(dir_path = dir_path, pattern = pattern)
  results <- list()
  for (module in modules) {
    for (name in names(module)) {
      results[[name]] <- module[[name]]
    }
  }
  results
}

box::export(load_test_modules)
