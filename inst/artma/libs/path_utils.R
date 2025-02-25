#' @export
get_script_path <- function() {
  script_path <- commandArgs(trailingOnly = TRUE)[1]
  return(script_path)
}

#' Ensure that a path (character) ends with a substring.
#'  Return a boolean indicating whether or not this is T.
#'
#' @param path [character] The path to check.
#' @param substr [character] The substring to check for.
#' @return [logical] TRUE if the path ends with the substring, FALSE otherwise.
#' @export
path_ends_with <- function(path, substr) {
  return(grepl(paste0(substr, "$"), path))
}

#' Set working directory based on a script path
#' @export
set_working_directory <- function(script_path = NA) {
  if (is.null(script_path)) {
    cat("Please provide the full path to the script.\n")
  } else {
    script_directory <- dirname(script_path)
    setwd(script_directory) # nolint
    cat("Working directory set to:", script_directory, "\n")
  }
}

#' Using a path in the PATHS object, return the full path.
#' @export
get_full_path <- function(path) {
  path <- normalizePath(path)
  path <- gsub("\\", "/", path, fixed = TRUE)
  path
}
