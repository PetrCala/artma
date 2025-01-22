GH_REPO_PATH <- "PetrCala/artma@dev"
remotes::install_github(GH_REPO_PATH, force = TRUE)
library(artma)

# Should pass without error
artma::artma()
