box::use(
  artma / const[CONST],
  artma / libs / core / validation[assert]
)

.find_package_root <- function(package_name, start = getwd()) {
  current <- tools::file_path_as_absolute(start)

  repeat {
    desc <- file.path(current, "DESCRIPTION")
    if (file.exists(desc)) {
      pkg_record <- tryCatch(
        base::read.dcf(desc, fields = "Package"),
        error = function(...) NULL
      )
      # [[ drops the "Package" dimname that [1, 1] keeps; a named value never passes identical()
      if (!is.null(pkg_record) && nrow(pkg_record) >= 1 && identical(pkg_record[[1, 1]], package_name)) {
        return(current)
      }
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }

    current <- parent
  }
}

.normalize_box_paths <- function(paths) {
  if (rlang::is_empty(paths)) {
    return(character())
  }
  normalizePath(paths, winslash = "/", mustWork = FALSE)
}

#' @return *\[character\]* The path to the folder where the main 'artma' folder is located. In case the package is installed, the path to the package folder is returned. In case the package is in development mode, the path to the 'inst' folder is returned instead.
#' @export
get_pkg_path <- function() {
  package_name <- CONST$PACKAGE_NAME

  box_paths <- .normalize_box_paths(getOption("box.path"))
  # Any box.path entry that actually contains the package's module directory
  # can serve modules and package files. Taking the first match respects the
  # search order (e.g. a worktree's inst/ prepended by tests/setup.R wins over
  # an .Rprofile entry pointing at another checkout).
  candidates <- box_paths[
    dir.exists(box_paths) & dir.exists(file.path(box_paths, package_name))
  ]
  if (!rlang::is_empty(candidates)) {
    return(candidates[[1]])
  }

  pkg_root <- .find_package_root(package_name)
  if (!is.null(pkg_root)) {
    inst_dir <- file.path(pkg_root, "inst")
    if (dir.exists(file.path(inst_dir, package_name))) {
      return(inst_dir)
    }
    return(pkg_root)
  }

  installed_path <- tryCatch(
    suppressWarnings(find.package(package_name, quiet = TRUE)),
    error = function(...) character()
  )
  if (!rlang::is_empty(installed_path) && dir.exists(installed_path[[1]])) {
    return(installed_path[[1]])
  }

  cli::cli_abort(cli::format_inline(
    "Failed to determine package path for {.pkg {package_name}}"
  ))
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

# Alternative to 'rappdirs' (https://github.com/r-lib/rappdirs)
DIR_USR_DATA <- tools::R_user_dir(CONST$PACKAGE_NAME, which = "data")
DIR_USR_CONFIG <- tools::R_user_dir(CONST$PACKAGE_NAME, which = "config")
DIR_USR_CACHE <- tools::R_user_dir(CONST$PACKAGE_NAME, which = "cache")

#' A list of paths used in the project
#'
#' @export
PATHS <- list(
  PACKAGE_PATH = PACKAGE_PATH,
  # Directories
  PROJECT_ROOT = PROJECT_ROOT,
  DIR_CONFIG = DIR_CONFIG,
  DIR_METHODS = DIR_METHODS,
  DIR_OPTIONS = DIR_OPTIONS,
  DIR_OPTIONS_TEMPLATES = DIR_OPTIONS_TEMPLATES,

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
