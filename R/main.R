#' @title artma main
#' @param options *\[character\]* Name of the user options file to use.
#' @param options_dir *\[character\]* Path to the directory that contains user options.
#' @param FUN *\[function\]* The function to be called after the setup.
#' @return *\[any\]* Depends on the main function definition.
#' @export
main <- function(
    options = NULL,
    options_dir = NULL,
    FUN = NULL) {
  if (is.null(FUN)) {
    FUN <- function() {
      cli::cli_inform("{.emph artma} main function")
    }
  }

  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options,
    options_dir = options_dir,
    FUN = FUN
  )
}
