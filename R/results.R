#' @title Get Results Directory Path
#' @description Returns the resolved path to the output directory where analysis
#'   results (tables, graphics) are saved. The path is printed and returned
#'   invisibly.
#' @param options *\[character, optional\]* Name of the options file (with or
#'   without `.yaml` extension). If `NULL` and running interactively, you will
#'   be prompted to select an options file.
#' @param options_dir *\[character, optional\]* Directory containing the options
#'   file. If `NULL`, uses the default options directory.
#' @return *\[character\]* The resolved output directory path (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Get the results directory
#' results.dir(options = "my_analysis.yaml")
#' }
results.dir <- function(options = NULL, options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options,
    options_dir = options_dir,
    FUN = function() {
      box::use(artma / output / export[resolve_output_dir])
      dir <- resolve_output_dir()
      cli::cli_inform("Results directory: {.path {dir}}")
      invisible(dir)
    }
  )
}

#' @title Open Results Directory
#' @description Opens the output directory in the system file browser (Finder
#'   on macOS, Explorer on Windows, or the default file manager on Linux).
#' @param options *\[character, optional\]* Name of the options file (with or
#'   without `.yaml` extension). If `NULL` and running interactively, you will
#'   be prompted to select an options file.
#' @param options_dir *\[character, optional\]* Directory containing the options
#'   file. If `NULL`, uses the default options directory.
#' @return *\[character\]* The resolved output directory path (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Open the results directory in your file browser
#' results.open(options = "my_analysis.yaml")
#' }
results.open <- function(options = NULL, options_dir = NULL) {
  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options,
    options_dir = options_dir,
    FUN = function() {
      box::use(artma / output / export[resolve_output_dir])
      dir <- resolve_output_dir()
      if (!dir.exists(dir)) {
        cli::cli_abort(c(
          "x" = "Results directory does not exist yet: {.path {dir}}",
          "i" = "Run {.code artma()} first to generate results."
        ))
      }
      os <- Sys.info()[["sysname"]]
      if (identical(os, "Darwin")) {
        system2("/usr/bin/open", dir)
      } else if (identical(os, "Windows")) {
        get("shell.exec", envir = baseenv())(dir)
      } else {
        system2("xdg-open", dir)
      }
      cli::cli_alert_success("Opened {.path {dir}}")
      invisible(dir)
    }
  )
}
