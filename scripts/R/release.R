# This file is a partial copy of the 'release.R' file from the devtools package for R.
# We use it here as the source package does not export a couple of functions we need for our release cycle.
# To see the source file, go to https://github.com/r-lib/devtools/blob/main/R/release.R

cran_submission_url <- "https://xmpalantir.wu.ac.at/cransubmit/index2.php"

as_object_size <- function(x) structure(x, class = "object_size") # nolint: undesirable_function_linter.

#' @export
cran_mirror <- function(repos = getOption("repos")) {
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"

  if (is.null(names(repos))) {
    names(repos) <- "CRAN"
  }

  repos[["CRAN"]]
}

#' Return the version of a package on CRAN (or other repository)
#' @param package The name of the package.
#' @param available A matrix of information about packages.
#' @export
cran_pkg_version <- function(package, available = available.packages()) {
  idx <- available[, "Package"] == package
  if (any(idx)) {
    as.package_version(available[package, "Version"])
  } else {
    NULL
  }
}

# has_cran_results <- function(pkg) {
#   cran_res <- foghorn::cran_results(
#     pkg = pkg,
#     show = c("error", "fail", "warn", "note")
#   )
#   sum(cran_res[, -1]) > 0
# }

#' Searches in codemeta.json
#' @export
find_release_questions <- function(pkg = ".") {
  pkg <- devtools::as.package(pkg)

  q_fun <- pkgload::ns_env(pkg$package)$release_questions
  if (is.null(q_fun)) {
    character()
  } else {
    q_fun()
  }
}

cran_comments <- function(pkg = ".", call = parent.frame()) {
  pkg <- devtools::as.package(pkg)

  path <- path(pkg$path, "cran-comments.md")
  if (!file.exists(path)) {
    cli::cli_warn(
      c(
        x = "Can't find {.file cran-comments.md}.",
        i = "This file is used to communicate your release process to the CRAN team.",
        i = "Create it with {.code use_cran_comments()}."
      ),
      call = call
    )
    return(character())
  }

  paste0(readLines(path, warn = FALSE), collapse = "\n")
}

extract_cran_msg <- function(msg) {
  # Remove "CRAN package Submission" and "Submit package to CRAN"
  msg <- gsub("CRAN package Submission|Submit package to CRAN", "", msg)

  # remove all html tags
  msg <- gsub("<[^>]+>", "", msg)

  # remove tabs
  msg <- gsub("\t+", "", msg)

  # Remove extra newlines
  msg <- gsub("\n+", "\n", msg)

  msg
}


get_maintainer <- function(pkg = ".") {
  pkg <- devtools::as.package(pkg)

  authors <- pkg$`authors@r`
  if (!is.null(authors)) {
    people <- eval(parse(text = authors))
    if (is.character(people)) {
      maintainer <- utils::as.person(people)
    } else {
      maintainer <- Find(function(x) "cre" %in% x$role, people)
    }
  } else {
    maintainer <- pkg$maintainer
    if (is.null(maintainer)) {
      cli::cli_abort("No maintainer defined in package.")
    }
    maintainer <- utils::as.person(maintainer)
  }

  list(
    name = paste(maintainer$given, maintainer$family),
    email = maintainer$email
  )
}

upload_cran <- function(pkg, built_path, call = parent.frame()) {
  pkg <- devtools::as.package(pkg)
  maint <- get_maintainer(pkg)
  comments <- cran_comments(pkg, call = call)

  # Initial upload ---------
  cli::cli_inform(c(i = "Uploading package & comments"))
  rlang::check_installed("httr")
  body <- list(
    pkg_id = "",
    name = maint$name,
    email = maint$email,
    uploaded_file = httr::upload_file(built_path, "application/x-gzip"),
    comment = comments,
    upload = "Upload package"
  )
  r <- httr::POST(cran_submission_url, body = body)

  # If a 404 likely CRAN is closed for maintenance, try to get the message
  if (httr::status_code(r) == 404) {
    msg <- ""
    try({
      r2 <- httr::GET(sub("index2", "index", cran_submission_url))
      msg <- extract_cran_msg(httr::content(r2, "text"))
    })
    cli::cli_abort(
      c(
        "*" = "Submission failed",
        "x" = msg
      ),
      call = call
    )
  }

  httr::stop_for_status(r)
  new_url <- httr::parse_url(r$url)

  # Confirmation -----------
  cli::cli_inform(c(i = "Confirming submission"))
  body <- list(
    pkg_id = new_url$query$pkg_id,
    name = maint$name,
    email = maint$email,
    policy_check = "1/",
    submit = "Submit package"
  )
  r <- httr::POST(cran_submission_url, body = body)
  httr::stop_for_status(r)
  new_url <- httr::parse_url(r$url)
  if (new_url$query$submit == "1") {
    cli::cli_inform(c(
      "v" = "Package submission successful",
      "i" = "Check your email for confirmation link."
    ))
  } else {
    cli::cli_abort("Package failed to upload.", call = call)
  }

  invisible(TRUE)
}


flag_release <- function(pkg = ".") {
  pkg <- devtools::as.package(pkg)

  cli::cli_inform(c("!" = "Don't forget to tag this release once accepted by CRAN"))

  withr::with_dir(pkg$path, {
    sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  })

  dat <- list(
    Version = pkg$version,
    Date = format(Sys.time(), tz = "UTC", usetz = TRUE),
    SHA = sha
  )

  write.dcf(dat, file = path(pkg$path, "CRAN-SUBMISSION"))
  usethis::use_build_ignore("CRAN-SUBMISSION")
}


#' @export
submit_cran <- function(pkg = ".", args = NULL) {
  pkg <- devtools::as.package(pkg)

  built_path <- pkgbuild::build(pkg$path, tempdir(), manual = TRUE, args = args)

  size <- format(as_object_size(fs::file_info(built_path)$size), units = "auto") # nolint: unused_declared_object_linter.
  cli::cat_rule("Submitting", col = "cyan")
  cli::cli_inform(c(
    "i" = "Path {.file {built_path}}",
    "i" = "File size: {size}"
  ))
  cli::cat_line()

  upload_cran(pkg, built_path)

  usethis::with_project(
    pkg$path,
    flag_release(pkg)
  )
}
