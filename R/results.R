#' Read the last export directory from the marker file
#'
#' @return The directory path, or `NULL` if the marker does not exist or the
#'   recorded directory no longer exists.
#' @keywords internal
read_last_export_dir <- function() {
  marker_path <- file.path(
    tools::R_user_dir("artma", which = "cache"),
    "last_export_dir"
  )

  if (!file.exists(marker_path)) {
    return(NULL)
  }

  dir_path <- tryCatch(
    readLines(marker_path, n = 1L, warn = FALSE),
    error = function(e) NULL
  )

  if (is.null(dir_path) || !nzchar(dir_path) || !dir.exists(dir_path)) {
    return(NULL)
  }

  dir_path
}

#' Open a directory in the system file browser
#'
#' @param dir *\[character\]* Path to the directory to open.
#' @return The directory path (invisibly).
#' @keywords internal
open_dir_in_browser <- function(dir) {
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

#' @title Get Results Directory Path
#' @description Returns the resolved path to the output directory where analysis
#'   results (tables, graphics) are saved. The path is printed and returned
#'   invisibly. When called without arguments, tries to use the most recently
#'   exported directory without prompting for an options file.
#' @param options *\[character, optional\]* Name of the options file (with or
#'   without `.yaml` extension). If `NULL`, the function first checks for a
#'   recent export marker. If no marker is found and running interactively,
#'   you will be prompted to select an options file.
#' @param options_dir *\[character, optional\]* Directory containing the options
#'   file. If `NULL`, uses the default options directory.
#' @return *\[character\]* The resolved output directory path (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Get the most recent results directory
#' results.dir()
#'
#' # Get results dir for a specific options file
#' results.dir(options = "my_analysis.yaml")
#' }
results.dir <- function(options = NULL, options_dir = NULL) {
  if (is.null(options) && is.null(options_dir)) {
    last_dir <- read_last_export_dir()
    if (!is.null(last_dir)) {
      cli::cli_inform("Results directory: {.path {last_dir}}")
      return(invisible(last_dir))
    }
  }

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
#'   When called without arguments, tries to open the most recently exported
#'   results directory without prompting for an options file.
#' @param options *\[character, optional\]* Name of the options file (with or
#'   without `.yaml` extension). If `NULL`, the function first checks for a
#'   recent export marker. If no marker is found and running interactively,
#'   you will be prompted to select an options file.
#' @param options_dir *\[character, optional\]* Directory containing the options
#'   file. If `NULL`, uses the default options directory.
#' @return *\[character\]* The resolved output directory path (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Open the most recent results (no prompt if a recent export exists)
#' results.open()
#'
#' # Open results for a specific options file
#' results.open(options = "my_analysis.yaml")
#' }
results.open <- function(options = NULL, options_dir = NULL) {
  if (is.null(options) && is.null(options_dir)) {
    last_dir <- read_last_export_dir()
    if (!is.null(last_dir)) {
      return(open_dir_in_browser(last_dir))
    }
  }

  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options,
    options_dir = options_dir,
    FUN = function() {
      box::use(artma / output / export[resolve_output_dir])
      dir <- resolve_output_dir()
      open_dir_in_browser(dir)
    }
  )
}
