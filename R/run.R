#' @export
run <- function(
    methods = NULL,
    options = NULL,
    options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options = options,
    options_dir = options_dir
  )

  box::use(
    artma / const[CONST]
  )

  if (is.null(methods)) {
    logger::log_debug("No runtime methods were provided. Running all available methods...")
    methods <- CONST$SUPPORTED_METHODS
  }
  logger::log_success("Done.")
}
