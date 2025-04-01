#' @title Crawl and import modules
#' @description Provided a path to a directory, crawl this directory for .R files and import all of them as box modules. Each such module will be imported under its name.
#' @details Assume you have a file 'my_custom_module.R' in a directory you want to import. Running this function on that directory loads a module named 'my_custom_module' into memory through box. Consequently, calls su
#' @param dir_path *\[character\]* Path to the directory to crawl.
#' `list` A list of box modules, accessible by their name.
#'
#' @usage
#' # Assume you have a 'my_custom_module.R' inside the following directory
#' custom_dir <- file.path('path', 'to', 'your', 'dir')
#'
#' modules <- crawl_and_import_modules(custom_dir)
#' my_custom_module <- modules[['my_custom_module']]
#'
#' # The following calls will work
#' my_custom_module$some_method()
#' my_custom_module$another_method()
#' modules$my_custom_module$some_method()
crawl_and_import_modules <- function(dir_path) {
  browser()
  if (!dir.exists(dir_path)) {
    rlang::abort(glue::glue("Non-existent directory when importing modules: {dir_path}"))
  }

  box::use(artma / libs / path[turn_path_into_box_import])

  modules <- list()

  # It is possible that the box imports may not work across all devices. Were that to be the case, consider using 'with_dir' to change the working dir temporarily for the following chunk of code.

  r_files <- list.files(path = dir_path, pattern = "\\.R$", full.names = FALSE)
  for (f in r_files) {
    module_name <- base::basename(tools::file_path_sans_ext(f))
    module_path <- tools::file_path_as_absolute(file.path(dir_path, f))

    box_import_statement <- turn_path_into_box_import(module_path)
    logger::log_debug(glue::glue("Running the following import statement: '{box_import_statement}'"))
    eval(box_import_statement) # Imports the module

    # Calling the module name should now return the module itself
    imported_module <- eval(parse(text = module_name))
    if (!inherits(imported_module, "box$mod")) {
      rlang::abort(glue::glue("Failed to import {module_name} as a box module. Aborting..."))
    }
    modules[[module_name]] <- imported_module
  }

  modules
}


#' @title Validate runtime method modules
#' @description Given a list of modules, validate that all of the components of this list are valid runtime method modules. This means ensuring they are proper module objects, contain the 'run' function, etc. Raise an error if any of these validations fail.
#' @param modules [list[box.module]] A list of modules to validate.
#' @return `NULL` Validates the object structure
validate_runtime_method_modules <- function(modules) { # nolint: object_length_linter.
  if (!is.list(modules)) {
    obj_class <- class(modules)
    rlang::abort(glue::glue("Invalid runtime method modules object: {modules}. Class: '{obj_class}'. Expected: 'list'."))
  }

  if (length(modules) == 0) {
    logger::log_warn("No runtime method modules to validate.")
    return(NULL)
  }

  for (module_name in names(modules)) {
    tryCatch(
      {
        main_method <- modules[[module_name]]$run # Can throw an error

        if (!inherits(main_method, "function")) {
          rlang::abort(glue::glue("Missing or invalid 'run' function in module '{module_name}'. Make sure your module contains this function."))
        }
      },
      error = function(e) {
        rlang::abort(glue::glue("Error validating the '{module_name}' runtime methods module: {e}"))
      }
    )
  }
}



box::export(
  crawl_and_import_modules,
  validate_runtime_method_modules
)
