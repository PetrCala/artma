# GH_REPO_PATH <- "PetrCala/artma@master"
# remotes::install_github(GH_REPO_PATH, force = TRUE)

artma_path <- dirname(dirname(getwd()))

if (!dir.exists(artma_path)) {
  cli::cli_abort("The path to the artma package is not valid: {artma_path}")
}

remotes::install_local(artma_path, force = TRUE)
library(artma) # nolint: undesirable_function_linter.

# Should pass without error (interactive mode will prompt for options)
# In non-interactive mode, this will return invisibly
artma::artma()
