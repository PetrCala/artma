#' @title ARTMA
#' @param options [character] Name of the user options file to use.
#' @param options_dir [character] Path to the directory that contains user options.
#' @export
artma <- function(
    options = NULL,
    options_dir = NULL) {
  runtime_setup(
    options = options,
    options_dir = options_dir
  )
}
