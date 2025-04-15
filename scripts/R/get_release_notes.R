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
start <- grep(paste0("<a name=\"", tag, "\">"), lines)
end <- grep("<a name=", lines)
end <- end[end > start][1] - 1

if (length(start) == 0) {
  stop(sprintf("Could not find start tag '%s' in NEWS.md", tag))
}

if (is.na(end)) {
  end <- length(lines)
}

writeLines(lines[start:end], "release-notes.md")
