#' @title ARTMA main
#' @param options [character] Name of the user options file to use.
#' @param options_dir [character] Path to the directory that contains user options.
#' @export
main <- function(
    options = NULL,
    options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options = options,
    options_dir = options_dir
  )
}
