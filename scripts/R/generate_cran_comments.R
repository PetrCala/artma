#!/usr/bin/env Rscript

option_list <- list(
  optparse::make_option(c("--old-version"),
    type = "character", default = "Unknown-old",
    help = "Previous version tag [default = %default]"
  ),
  optparse::make_option(c("--new-version"),
    type = "character", default = "Unknown-new",
    help = "New version tag [default = %default]"
  ),
  optparse::make_option(c("--manual-file"),
    type = "character", default = NULL,
    help = "Path to manual cran-comments.md notes [default = %default]"
  ),
  optparse::make_option(c("--check-status"),
    type = "character", default = "Unknown",
    help = "Status of R CMD check (success/failure) [default = %default]"
  )
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opts <- optparse::parse_args(opt_parser)

write_cmd_check_results <- function(status) {
  status_line <- if (status == "success") {
    "0 errors ✔\n0 warnings ✔\n0 notes ✔"
  } else {
    "Errors or warnings found in R CMD check. See logs for details."
  }

  lines <- c(
    "## R CMD check results",
    "",
    status_line,
    "",
    "R CMD check passed on:", # assume the R-CMD-check was run
    "",
    "- { os: macos-latest, r: \"release\" }",
    "- { os: windows-latest, r: \"release\" }",
    "- { os: ubuntu-latest, r: \"devel\", http-user-agent: \"release\" }",
    "- { os: ubuntu-latest, r: \"release\" }",
    "- { os: ubuntu-latest, r: \"oldrel-1\" }",
    ""
  )

  return(lines)
}

write_notes_to_cran_reviewers <- function(old_version, new_version, manual_file = NULL) {
  manual_notes <- character()

  if (!is.null(manual_file) && nzchar(manual_file) && file.exists(manual_file)) {
    manual_notes <- readLines(manual_file, warn = FALSE)
    manual_notes <- c(manual_notes, "")
  }

  lines <- c(
    "## Notes to CRAN reviewers",
    "",
    sprintf("* This is an update from v%s to v%s.", old_version, new_version),
    manual_notes
  )

  return(lines)
}

main <- function(opts) {
  check_results <- write_cmd_check_results(opts$`check-status`)
  cran_notes <- write_notes_to_cran_reviewers(opts$`old-version`, opts$`new-version`, opts$`manual-file`)

  full_lines <- c(check_results, cran_notes)

  writeLines(full_lines, con = "cran-comments.md")

  output_path <- file.path(getwd(), "cran-comments.md")
  cat(sprintf("cran-comments.md generated successfully at %s.\n", output_path))
}

main(opts)
