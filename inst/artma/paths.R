box::use(
  artma / const[CONST]
)


#' @returns The path to the folder where the main 'artma' folder is located. In case the package is installed, the path to the package folder is returned. In case the package is in development mode, the path to the 'inst' folder is returned instead.
#' @export
get_pkg_path <- function() {
  package_name <- CONST$PACKAGE_NAME
  box_path <- getOption("box.path")
  dev_path <- grep(file.path(package_name, "inst$"), box_path, value = TRUE)

  is_dev <- dir.exists(dev_path)
  if (is_dev) {
    return(dev_path)
  }
  return(grep(glue::glue("{package_name}$"), box_path, value = TRUE))
}

PACKAGE_PATH <- get_pkg_path()
PROJECT_ROOT <- file.path(PACKAGE_PATH, CONST$PACKAGE_NAME)
DIR_CONFIG <- file.path(PROJECT_ROOT, "config")
DIR_OPTIONS <- file.path(PROJECT_ROOT, "options")
DIR_OPTIONS_TEMPLATES <- file.path(DIR_OPTIONS, "templates")
DIR_TEMP <- file.path(PROJECT_ROOT, "temp")

#' A list of paths used in the project
#'
#' @export
PATHS <- list(
  # Directories
  PROJECT_ROOT = PROJECT_ROOT,
  DIR_CONFIG = DIR_CONFIG,
  DIR_OPTIONS = DIR_OPTIONS,
  DIR_OPTIONS_TEMPLATES = DIR_OPTIONS_TEMPLATES,
  DIR_TEMP = DIR_TEMP,
  DIR_LOGS = file.path(DIR_TEMP, "logs"),
  DIR_CACHE = file.path(DIR_TEMP, "cache"),
  DIR_USER_OPTIONS = file.path(DIR_TEMP, "options"), # Store user options here by default

  # Files
  FILE_OPTIONS_TEMPLATE = file.path(DIR_OPTIONS_TEMPLATES, "options_template.yaml")
)
