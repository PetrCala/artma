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

  if (is.null(methods)) {
    methods <- utils::select.list(
      title = "No runtime methods were provided. Please select the methods you would like to run: ",
      choices = CONST$SUPPORTED_METHODS,
      multiple = TRUE
    )
    if (is_empty(methods)) {
      stop("No runtime methods were selected. Aborting...")
    }
  }

  if (!(is.character(methods) && all(methods %in% CONST$SUPPORTED_METHODS))) {
    rlang::abort(paste("Invalid runtime methods selected:", paste(as.character(methods), collapse = ", "), "\nTo see a list of available methods, run 'artma::methods.list()'."))
  }

  logger::log_success("Done.")
}
