#!/usr/bin/env Rscript

#' This script serves as an entrypoint to invoke the R actions either from the command line,
#' or from an interactive R session.
#'
#' Usage
#' - From a command line, run `Rscript run <action> <args>`
#'  or
#' - Specify the desired action in options.json and run the script inside an R session.

# Environment preparation
rm(list = ls())
is_testing <- Sys.getenv("TESTTHAT") == "true"
options(scipen = 999) # No scientific notation
Sys.setenv(PATH = paste("/bin", Sys.getenv("PATH"), sep = ":")) # For binary files


# Static
run_dir <- "meta-facilitator/R"
new_dir <- NULL
does_path_match <- function(path) grepl(paste0(run_dir, "$"), getwd())

# Ensure the correct working directory regardless of invocation type
if (is_testing) {
  logger::log_debug("Running in testing mode")
  new_dir <- getwd()
} else if (exists(".vsc.getSession")) {
  logger::log_debug("Running in VS Code")
  session <- .vsc.getSession()
  new_dir <- dirname(session$file)
} else if (interactive()) {
  logger::log_debug("Running in interactive mode") # Assume RStudio
  current_file <- suppressWarnings(rstudioapi::getActiveDocumentContext()$path)
  if (!does_path_match(dirname(current_file))) {
    # Handle cases when the current file is not the one open (such as in VSCodee) - get currently executed scripts' location
    current_file <- normalizePath(sys.frame(1)$ofile)
  }
  new_dir <- dirname(current_file)
} else {
  logger::log_debug("Running in non-interactive mode")
  args <- commandArgs(trailingOnly = FALSE)
  # Iterate through arguments to find the "--file" followed by the script path
  script_path <- NULL
  for (i in seq_along(args)) {
    if (grepl("--file", args[i])) {
      script_path <- sub("^--file=", "", args[i]) # The script path points to the 'r' folder
      break
    }
  }
  if (is.null(script_path)) {
    rlang::abort("Could not find the script path in the arguments. Try specifying the script path using --file=<path> argument.")
  }
  new_dir <- dirname(script_path)
}
setwd(new_dir) # Fails if any of the conditions fail to attribute a new_dir


# Check that the current WD ends with the expected directory substring
# In testing, the WD does not have to match the expected directory
run_dir_matches <- grepl(paste0(run_dir, "$"), getwd())
if (!run_dir_matches && !is_testing) {
  rlang::abort("Failed to set the working directory to the correct location. Try running the script again.")
}

# Set up the path for module imports
options(box.path = getwd())

# Relative paths are sourced only after the WD is set correctly
box::use(
  base / options[load_options],
  # libs / logs / index[setup_logging],
  actions / index[ACTIONS, run_tests],
  actions / utils[validate_action, get_invocation_args]
)

# load_options() # Load custom options into the namespace
# setup_logging() # Requires options to be loaded

# logger::log_info("Done")

args <- get_invocation_args()

do.call(ACTIONS[[args$action]], args$run_args)
