box::use(
  artma / const[CONST],
  artma / libs / validation[assert]
)


#' @return *\[character\]* The path to the folder where the main 'artma' folder is located. In case the package is installed, the path to the package folder is returned. In case the package is in development mode, the path to the 'inst' folder is returned instead.
#' @export
get_pkg_path <- function() {
  package_name <- CONST$PACKAGE_NAME
  box_path <- getOption("box.path")
  dev_path <- grep(file.path(package_name, "inst$"), box_path, value = TRUE)

  if (any(dir.exists(dev_path))) {
    if (is.vector(dev_path)) {
      return(dev_path[1])
    }
    return(dev_path)
  }

  return(grep(glue::glue("{package_name}$"), box_path, value = TRUE))
}

PACKAGE_PATH <- get_pkg_path()
assert(
  is.character(PACKAGE_PATH) && length(PACKAGE_PATH) == 1,
  "Package path must be a single character string"
)

PROJECT_ROOT <- file.path(PACKAGE_PATH, CONST$PACKAGE_NAME)
DIR_CONFIG <- file.path(PROJECT_ROOT, "config")
DIR_METHODS <- file.path(PROJECT_ROOT, "methods")
DIR_OPTIONS <- file.path(PROJECT_ROOT, "options")
DIR_OPTIONS_TEMPLATES <- file.path(DIR_OPTIONS, "templates")
DIR_TESTING <- file.path(PROJECT_ROOT, "testing")
DIR_USR_DATA <- tools::R_user_dir(CONST$PACKAGE_NAME, which = "data")
DIR_USR_CONFIG <- tools::R_user_dir(CONST$PACKAGE_NAME, which = "config")
DIR_USR_CACHE <- tools::R_user_dir(CONST$PACKAGE_NAME, which = "cache")

#' A list of paths used in the project
#'
#' @export
PATHS <- list(
  # Directories
  PROJECT_ROOT = PROJECT_ROOT,
  DIR_CONFIG = DIR_CONFIG,
  DIR_METHODS = DIR_METHODS,
  DIR_OPTIONS = DIR_OPTIONS,
  DIR_OPTIONS_TEMPLATES = DIR_OPTIONS_TEMPLATES,
  DIR_TESTING = DIR_TESTING,
  DIR_MOCKS = file.path(DIR_TESTING, "mocks"),
  DIR_FIXTURES = file.path(DIR_TESTING, "fixtures"),

  # Persistent user data directories (alternative to 'rappdirs')
  DIR_USR_DATA = DIR_USR_DATA,
  DIR_USR_CONFIG = DIR_USR_CONFIG,
  DIR_USR_CACHE = DIR_USR_CACHE,
  DIR_USR_DATA_TMP = file.path(DIR_USR_DATA, "tmp"),

  # Files
  FILE_OPTIONS_TEMPLATE = file.path(DIR_OPTIONS_TEMPLATES, "options_template.yaml"),
  FILE_MOCKS_TMP_DATA = file.path(DIR_USR_DATA, CONST$MOCKS$TMP_DATA_FILE_NAME),
  FILE_MOCKS_TMP_OPTIONS = file.path(DIR_USR_CONFIG, CONST$MOCKS$TMP_OPTIONS_FILE_NAME)
)
