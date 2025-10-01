#' Helper functions for working with user option files.
#'
#' These helpers centralize common file-system logic that was previously
#' scattered across the exported `artma::options.*` functions.  Keeping the
#' path resolution and YAML IO concerns together makes the higher-level
#' functions easier to read and reason about while preserving their behaviour.

#' @title Resolve the options template path
#' @description Returns the path to the options template file, validating that
#'   it exists.  Falls back to the package default when `template_path` is
#'   `NULL`.
#' @param template_path *[character, optional]* Path to an options template.
resolve_template_path <- function(template_path = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / libs / validation[assert_options_template_exists]
  )

  resolved <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  assert_options_template_exists(resolved)
  resolved
}

#' @title Resolve the options directory
#' @description Normalises the options directory, defaulting to the package
#'   location when `options_dir` is `NULL`.  Optionally enforces that the
#'   directory already exists.
#' @param options_dir *[character, optional]* A directory containing user
#'   options.
#' @param must_exist *[logical]* Whether the directory must exist.
resolve_options_dir <- function(options_dir = NULL, must_exist = TRUE) {
  box::use(artma / paths[PATHS])

  resolved <- options_dir %||% PATHS$DIR_USR_CONFIG

  if (isTRUE(must_exist) && !dir.exists(resolved)) {
    cli::cli_abort(
      glue::glue("The following options directory does not exist: {resolved}")
    )
  }

  resolved
}

#' @title Build the path to an options file
#' @param options_dir *[character]* Directory that contains user options.
#' @param options_file_name *[character]* File name of the options file.
options_file_path <- function(options_dir, options_file_name) {
  file.path(options_dir, options_file_name)
}

#' @title Read a YAML options file
#' @description Wraps `yaml::read_yaml()` with friendlier error reporting.
#' @param path *[character]* Full path to a YAML file.
read_options_file <- function(path) {
  tryCatch(
    yaml::read_yaml(path),
    error = function(err) {
      cli::cli_abort(
        "Failed to read the options file {.path {path}}: {err$message}"
      )
    }
  )
}

#' @title Write a YAML options file
#' @description Ensures the destination directory exists before writing the
#'   supplied options list.
#' @param path *[character]* Path to the YAML file to create.
#' @param options *[list]* Options to write.
write_options_file <- function(path, options) {
  box::use(artma / libs / file_utils[ensure_folder_existence])

  ensure_folder_existence(dirname(path))
  yaml::write_yaml(options, path)
}

#' @title List options files within a directory
#' @description Returns the names (or verbose labels) of YAML options files in
#'   `options_dir`.
#' @param options_dir *[character]* Directory to scan.
#' @param should_return_verbose_names *[logical]* Whether to return the
#'   `general$name` entry from each YAML file instead of the file name.
list_options_files <- function(options_dir, should_return_verbose_names = FALSE) {
  box::use(
    artma / const[CONST],
    artma / libs / utils[get_verbosity]
  )

  if (!dir.exists(options_dir)) {
    return(character(0))
  }

  file_paths <- list.files(
    path = options_dir,
    pattern = CONST$PATTERNS$YAML_FILES$REGEX,
    full.names = TRUE
  )

  if (length(file_paths) == 0) {
    return(character(0))
  }

  # Filter out template definitions so the caller only sees user-provided options.
  is_template_file <- function(path) {
    known_template_names <- CONST$OPTIONS$TEMPLATE_NAMES
    if (tolower(basename(path)) %in% known_template_names) {
      return(TRUE)
    }

    content <- tryCatch(
      read_options_file(path),
      error = function(...) NULL
    )

    if (!is.list(content) || length(content) == 0) {
      return(FALSE)
    }

    pending <- list(content)

    while (length(pending) > 0) {
      current <- pending[[1]]
      pending <- pending[-1]

      if (!is.list(current)) {
        next
      }

      current_names <- names(current)
      if (!is.null(current_names) && "type" %in% current_names) {
        if (any(CONST$OPTIONS$RECOGNIZED_KEYWORDS %in% current_names)) {
          return(TRUE)
        }
      }

      pending <- c(pending, current)
    }

    FALSE
  }

  keep_idx <- !vapply(
    X = file_paths,
    FUN = is_template_file,
    FUN.VALUE = logical(1)
  )

  file_paths <- file_paths[keep_idx]

  if (length(file_paths) == 0) {
    return(character(0))
  }

  if (!isTRUE(should_return_verbose_names)) {
    return(basename(file_paths))
  }

  option_names <- vector(mode = "character")
  for (file_name in file_paths) {
    option_name <- tryCatch(
      {
        if (get_verbosity() >= 4) {
          cli::cli_inform("Reading the options file {.path {file_name}}")
        }
        read_options_file(file_name)$general$name
      },
      error = function(cond) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning(
            "Failed to read the following options file: {.path {file_name}}"
          )
        }
        NULL
      }
    )
    option_names <- append(option_names, option_name)
  }

  option_names
}

box::export(
  list_options_files,
  options_file_path,
  read_options_file,
  resolve_options_dir,
  resolve_template_path,
  write_options_file
)
