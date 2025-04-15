#! /usr/bin/env Rscript

# Get the release notes for a given tag - used to create the release notes for a new release

option_list <- list(
  optparse::make_option(c("--new-version"),
    type = "character",
    default = "Unknown",
    help = "New version to get release notes for [default = %default]",
    dest = "new_version"
  )
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opts <- optparse::parse_args(opt_parser)

new_version <- opts$new_version
tag <- paste0("v", new_version)
lines <- readLines("NEWS.md")
anchor_line <- grep(paste0("<a name=\"", tag, "\">"), lines)
if (length(anchor_line) == 0) {
  stop(sprintf("Could not find anchor tag '<a name=\"%s\">' in NEWS.md", tag))
}

# The actual content starts five lines after the anchor: link, empty line, date, empty line, then content
start <- anchor_line + 5
end_candidates <- grep("<a name=", lines)
end <- end_candidates[end_candidates > anchor_line][1] - 1
if (is.na(end)) {
  end <- length(lines)
}

if (start > end) {
  warning("No release notes found for this version.")
  writeLines(character(0), "release-notes.md")
} else {
  writeLines(lines[start:end], "release-notes.md")
}
