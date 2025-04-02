#!/usr/bin/env Rscript
# bump_version.R

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("No semver level provided. Must be one of 'patch', 'minor', or 'major'.")
}

semver_level <- args[1]

# Source your existing R file that contains bump_version()
source("bump_version.R")

pkg_repo <- here::here("..", "..", "..", "..", "artma")
if (!dir.exists(pkg_repo)) {
  stop("The package repository does not exist under path", pkg_repo)
}

new_version <- bump_version(semver_level = semver_level, pkg.repo = pkg_repo)

cat(sprintf("New version is: %s\n", new_version))
