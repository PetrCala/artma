#' Bump Package 'Version:' and 'Date:' in DESCRIPTION File
#'
#' @description
#' This function let's you bump the version number and creation date of your
#' package's `DESCRIPTION` file. Supported versioning system is
#' [MAJOR.MINOR.PATCH](https://semver.org/).
#'
#' @param element `character`, one of `"major"`, `"minor"`, `"patch"` (default)
#'   to be bumped.
#' @param pkg.repo Path to package repository folder. Default is current working
#'   directory, i.e. `"."`.
#'
#' @seealso
#' \url{https://semver.org/}
#'
#' @export
bump_version <- function(element = "patch", pkg.repo = ".") {
  ### DESCRIPTION file =========================================================
  desc <- readLines(file.path(pkg.repo, "DESCRIPTION"))

  old.ver <- substr(
    grep("Version*", desc, value = TRUE), 10L,
    nchar(grep("Version*", desc, value = TRUE))
  )
  old <- as.numeric(unlist(strsplit(old.ver, ".", fixed = TRUE)))
  new.v <- switch(element,
    major = c(old[1L] + 1L, 0L, 0L),
    minor = c(old[1L], old[2L] + 1L, 0L),
    patch = c(old[1L], old[2L], old[3L] + 1L)
  )
  new.ver <- paste(new.v[1L], new.v[2L], new.v[3L], sep = ".")
  new.v <- new.v[1L] * 100L + new.v[2L] * 10L + new.v[3L]
  old <- old[1L] * 100L + old[2L] * 10L + old[3L]

  desc[grep("^Version", desc)] <- paste0("Version: ", new.ver)
  desc[grep("^Date", desc)] <- paste0("Date: ", Sys.Date())

  writeLines(desc, file.path(pkg.repo, "DESCRIPTION"))

  # ### pkg.name-package.Rd file - if present ====================================
  # pkg.name <- substr(desc[grep("^Package:", desc)], 10,
  #                    nchar(desc[grep("^Package:", desc)]))
  # pkg_fl = paste(pkg.repo, "man",
  #                paste(pkg.name, "-package.Rd", sep = ""),
  #                sep = "/")

  # if (file.exists(pkg_fl)) {
  #   pkg.doc <- readLines(pkg_fl)
  #   pkg.doc[grep("^Version", pkg.doc)] <- paste("Version: \\tab ",
  #                                               new.ver, "\\cr", sep = "")
  #   pkg.doc[grep("^Date", pkg.doc)] <- paste("Date: \\tab ",
  #                                            Sys.Date(), "\\cr", sep = "")
  #   writeLines(pkg.doc, paste(pkg.repo, "man",
  #                             paste(pkg.name, "-package.Rd", sep = ""),
  #                             sep = "/"))
  # }
}
