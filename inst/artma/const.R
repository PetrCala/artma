PACKAGE_NAME <- "artma"
PACKAGE_NAME_VERBOSE <- "Automatic Replication Tools for Meta-Analysis"

#' @returns The path to the folder where the main 'artma' folder is located. In case the package is installed, the path to the package folder is returned. In case the package is in development mode, the path to the 'inst' folder is returned instead.
#' @export
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
PROJECT_ROOT <- glue::glue("{PACKAGE_PATH}/{PACKAGE_NAME}")
OPTIONS_DIR_PATH <- glue::glue("{PROJECT_ROOT}/options")

#' @export
CONST <- list(
  # Base
  PACKAGE_NAME = PACKAGE_NAME,
  PACKAGE_NAME_VERBOSE = PACKAGE_NAME_VERBOSE,
  PACKAGE_PATH = PACKAGE_PATH,
  PROJECT_ROOT = PROJECT_ROOT, # Where the package modules are located
  OPTIONS_DIR_PATH = OPTIONS_DIR_PATH,

  # File paths
  OPTIONS_FILE_PATH = glue::glue("{OPTIONS_DIR_PATH}/options.yaml")
)
