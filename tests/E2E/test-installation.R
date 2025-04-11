# GH_REPO_PATH <- "PetrCala/artma@master"
# remotes::install_github(GH_REPO_PATH, force = TRUE)

artma_path <- dirname(dirname(getwd()))

if (!dir.exists(artma_path)) {
  stop(paste("The path to the artma package is not valid:", artma_path))
}

remotes::install_local(artma_path, force = TRUE)
library(artma)

# Should pass without error
artma::main()
