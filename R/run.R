#' @title Invoke methods
#' @description Pass a vector of runtime methods to invoke, together with a data frame to invoke these methods on, and invoke them.
#' @param method [character] A character vector of the methods to invoke. Can be NULL.
#' @param df [data.frame] The data frame to invoke the methods on.
#' @return [list] Results of the invocations, indexed by method names.
#'
#' @example
#' df <- data.frame(...)
#' invoke_runtime_methods(c("funnel_plot", "bma"), df)
#'
#' @keywords internal
invoke_runtime_methods <- function(methods, df, ...) {
  box::use(
    artma / libs / utils[is_empty]
  )

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

#' @export
run <- function(
    methods = NULL,
    options_file_name = NULL,
    options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir
  )
  results <- invoke_runtime_methods(methods = methods, df = NULL)

  logger::log_success("Done.")
}
