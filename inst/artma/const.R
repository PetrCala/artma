PACKAGE_NAME <- "artma"
PACKAGE_NAME_VERBOSE <- "Automatic Replication Tools for Meta-Analysis"

#' @returns The path to the folder where the package inst main modules (inst)are installed
get_pkg_path <- function() {
  box_path <- getOption("box.path")
  dev_path <- box_path[grepl(glue::glue("{PACKAGE_NAME}/inst$"), box_path)]

  is_dev <- ifelse(dir.exists(dev_path), TRUE, FALSE)
  if (is_dev) {
    return(dev_path)
  }
  return(box_path[grepl(glue::glue("{PACKAGE_NAME}$"), box_path)])
}

PACKAGE_PATH <- get_pkg_path()

#' @export
CONST <- list(
  # Base
  PACKAGE_NAME = PACKAGE_NAME,
  PACKAGE_NAME_VERBOSE = PACKAGE_NAME_VERBOSE,
  PACKAGE_PATH = PACKAGE_PATH,

  # File names
  OPTIONS_FILE_NAME = "options.yaml"
)
