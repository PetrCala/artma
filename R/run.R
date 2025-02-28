#' @export
run <- function(
    methods = NULL,
    options_file_name = NULL,
    options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir
  )

  box::use(
    artma / const[CONST],
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

  for (i in seq_along(supported_methods)) {
    method_name <- methods[i]
    if (method_name %in% methods) {
      logger::log_info(glue::glue("Running the '{method_name}' method..."))
      RUNTIME_METHOD_MODULES[[method_name]]$run()
    }
  }
  logger::log_success("Done.")
}
