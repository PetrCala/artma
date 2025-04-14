#! /usr/bin/env Rscript

# Get the release notes for a given tag - used to create the release notes for a new release

option_list <- list(
  optparse::make_option(c("--new-version"),
    type = "character", default = "Unknown",
    help = "New version to get release notes for [default = %default]"
  )
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opts <- optparse::parse_args(opt_parser)

new_version <- opts$new_version
tag <- paste0("v", new_version)
lines <- readLines("NEWS.md")
start <- grep(paste0("<a name=\"", tag), lines)
end <- grep("<a name=", lines)
end <- end[end > start][1] - 1
writeLines(lines[start:end], "release_notes.md")
