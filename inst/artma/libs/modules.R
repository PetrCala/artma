#' @export
crawl_and_import_modules <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    rlang::abort(glue::glue("Non-existent directory when importing modules: {dir_path}"))
  }

  # Create a list to hold the imported modules
  modules <- list()

  # Temporarily switch to the target directory so that box::use() sees files as modules
  withr::with_dir(dir_path, {
    # TODO
    # Find all .R files
    r_files <- list.files(pattern = "\\.R$", full.names = FALSE)

    # For each R file, import it via box::use and store the module
    for (f in r_files) {
      module_name <- tools::file_path_sans_ext(f) # Strip the .R extension
      # Build a small expression like box::use(./my_module)
      import_expr <- parse(text = sprintf("box::use(./%s)", module_name))

      # Evaluate the expression (imports the module)
      mod <- eval(import_expr)

      # Store in our modules list
      modules[[module_name]] <- mod
    }
  })


  # Return the named list of modules
  return(modules)
}
