#' @title Invoke methods
#' @description Pass a vector of runtime methods to invoke, together with a data frame to invoke these methods on, and invoke them.
#' @param methods *\[character\]* A character vector of the methods to invoke.
#' @param df *\[data.frame\]* The data frame to invoke the methods on.
#' `list` Results of the invocations, indexed by method names.
#' @param ... *\[any\]* Additional arguments to pass to the methods.
#'
#' Internal example:
#' df <- data.frame(...)
#' invoke_runtime_methods(c("funnel_plot", "bma"), df)
#'
#' @keywords internal
invoke_runtime_methods <- function(methods, df, ...) {
  box::use(
    artma / libs / utils[is_empty]
  )

  RUNTIME_METHOD_MODULES <- get_runtime_method_modules() # nolint: box_usage_linter.
  supported_methods <- names(RUNTIME_METHOD_MODULES)

  if (is.null(methods)) {
    methods <- utils::select.list(
      title = "No runtime methods were provided. Please select the methods you would like to run: ",
      choices = supported_methods,
      multiple = TRUE
    )
    if (is_empty(methods)) {
      stop("No runtime methods were selected. Aborting...")
    }
  }

  if (!(is.character(methods) && all(methods %in% supported_methods))) {
    rlang::abort(paste("Invalid runtime methods selected:", glue::glue_collapse(as.character(methods), sep = ", "), "\nTo see a list of available methods, run 'artma::methods.list()'."))
  }

  logger::log_info("Running the main ARTMA function.")
  logger::log_info(glue::glue("Invoking {length(methods)} methods..."))

  results <- list()
  for (i in seq_along(supported_methods)) {
    method_name <- methods[i]
    if (method_name %in% methods) {
      logger::log_info(glue::glue("Running the '{method_name}' method..."))
      results[[method_name]] <- RUNTIME_METHOD_MODULES[[method_name]]$run(df = df, ...)
    }
  }
  results
}

#' @title Run ARTMA
#' @description Run ARTMA with the specified methods and options.
#' @param methods *\[character, optional\]* A character vector of the methods to invoke. Defaults to NULL.
#' @param options_file_name *\[character, optional\]* The name of the options file to use. Defaults to NULL.
#' @param options_dir *\[character, optional\]* The directory containing the options file. Defaults to NULL.
#' @return *\[list\]* Results of the invocations, indexed by method names.
#' @export
run <- function(
    methods = NULL,
    options_file_name = NULL,
    options_dir = NULL) {
  main <- function() {
    box::use(artma / data / index[prepare_data])

    df <- prepare_data()
    results <- invoke_runtime_methods(methods = methods, df = df)
    logger::log_success("Done.")
  }

  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = main
  )
}
