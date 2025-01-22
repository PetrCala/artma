#' @title Packages
#' @description
#' This objects contains lists of packages that are used in the project.
#' Contents: CORE, DEV, INITIAL, NON_ATTACHED
#' @export
PACKAGES <- list(
  # A list of packages used in the project
  CORE = list(
    "cachem" = "1.0.8",
    "data.table" = "1.15.4",
    "dplyr" = NA,
    "logger" = NA,
    "magrittr" = NA, # The pipe module - part of dplyr
    "memoise" = "2.0.1",
    "metafor" = NA,
    "optparse" = NA,
    "plm" = "2.6-4",
    "readr" = "2.1.5",
    "readxl" = "0.1.1",
    "rlang" = "1.1.3",
    "stringr" = NA,
    "testthat" = "3.2.1",
    "tidyr" = NA,
    "yaml" = "2.3.8",
    "writexl" = NA
  ),
  # A list of packages that are used for development
  DEV = list(
    "precommit" = NA
  ),
  # A list of packages that are necessary for the environment preparation
  INITIAL = c(
    "languageserver",
    "box",
    "rstudioapi",
    "remotes",
    "pbapply"
  ),
  # A list of packages that are not attached to the search path
  NON_ATTACHED = c(
    "box"
  )
)
