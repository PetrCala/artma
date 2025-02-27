#' Reads the current package version from the DESCRIPTION file and returns it as a string
#' @keywords internal
get_current_version <- function(pkg.repo) {
  desc <- readLines(file.path(pkg.repo, "DESCRIPTION"))

  current.ver <- substr(
    grep("Version*", desc, value = TRUE), 10,
    nchar(grep("Version*", desc, value = TRUE))
  )
  current.ver
}

#' Given a semver string and a semver level, bump that string to the new semver level and return the modified string
#' @param current_version [character] The current version, given as a semver string.
#' @param semver_level [character] The new semver level.
#' @returns [character] The new version
#' @keywords internal
generate_new_version <- function(current_version, semver_level) {
  old <- as.numeric(unlist(strsplit(current_version, ".", fixed = TRUE)))
  new.v <- switch(semver_level,
    major = c(old[1] + 1, 0, 0),
    minor = c(old[1], old[2] + 1, 0),
    patch = c(old[1], old[2], old[3] + 1)
  )
  new.ver <- paste(new.v[1], new.v[2], new.v[3], sep = ".")
  new.ver
}

#' Bump Package 'Version:' and 'Date:' in DESCRIPTION File
#'
#' @description
#' This function let's you bump the version number and creation date of your
#' package's `DESCRIPTION` file. Supported versioning system is
#' [MAJOR.MINOR.PATCH](https://semver.org/).
#'
#' @param semver_level `character`, one of `"major"`, `"minor"`, `"patch"` (default)
#'   to be bumped.
#' @param pkg.repo Path to package repository folder. Default is current working
#'   directory, i.e. `"."`.
#' @return [character] The new version as a semver string
#'
#' @seealso
#' \url{https://semver.org/}
#'
#'
#' @export
bump_version <- function(semver_level = "patch", pkg.repo = ".") {
  # Validate input and file paths
  valid_semver_levels <- c("patch", "minor", "major")
  if (!semver_level %in% valid_semver_levels) {
    rlang::abort("Invalid semver level:", semver_level, "Must be one of:", paste(valid_semver_levels, sep = ", "))
  }

  options_file_path <- file.path(pkg.repo, "inst", "artma", "options", "templates", "options_template.yaml")
  if (!file.exists(options_file_path)) {
    stop("The options file template does not exist under path", options_file_path)
  }

  ### Generate a new package version
  old.ver <- get_current_version(pkg.repo = pkg.repo)
  new.ver <- generate_new_version(current_version = old.ver, semver_level = semver_level)
  print_info <- function(file_name) {
    print(glue::glue("Modifying the {file_name} package version: {old.ver} -> {new.ver}"))
  }

  ### Change the DESCRIPTION_FILE
  desc <- readLines(file.path(pkg.repo, "DESCRIPTION"))

  desc[grep("^Version", desc)] <- paste0("Version: ", new.ver)
  desc[grep("^Date", desc)] <- paste0("Date: ", Sys.Date())

  print_info(file_name = "DESCRIPTION")
  writeLines(desc, file.path(pkg.repo, "DESCRIPTION"))

  ### Change the options template
  opt <- readLines(options_file_path)

  old_version_str <- glue::glue("    default: \"{old.ver}\"")
  new_version_str <- glue::glue("    default: \"{new.ver}\"")
  opt[grep(old_version_str, opt)] <- new_version_str

  print_info(file_name = "options template")
  writeLines(opt, options_file_path)

  new.ver
}
