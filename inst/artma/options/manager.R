box::use(
  artma / const[CONST],
  artma / paths[PATHS],
  artma / options / template[flatten_template_options, read_template],
  artma / options / utils[get_expected_type, validate_option_value],
  artma / libs / validation[assert, validate, assert_options_template_exists],
  artma / libs / utils[get_verbosity]
)

#' Resolve the options directory for downstream operations.
#'
#' @param options_dir *[character]* Optional override for the options directory.
#' @param must_exist *[logical]* Whether to abort when the directory does not exist.
#' @param create_if_missing *[logical]* Whether the directory should be created when
#'   it is missing and `must_exist` is `FALSE`.
#' @return *[character]* Normalized path to the options directory.
resolve_options_dir <- function(
    options_dir = NULL,
    must_exist = TRUE,
    create_if_missing = FALSE) {
  validate(is.null(options_dir) || (is.character(options_dir) && length(options_dir) == 1))

  resolved_dir <- if (is.null(options_dir)) PATHS$DIR_USR_CONFIG else options_dir

  if (dir.exists(resolved_dir)) {
    return(resolved_dir)
  }

  if (create_if_missing) {
    dir.create(resolved_dir, recursive = TRUE, showWarnings = FALSE)
    return(resolved_dir)
  }

  if (must_exist) {
    cli::cli_abort(cli::format_inline(
      "The following options directory does not exist: {.path {resolved_dir}}"
    ))
  }

  resolved_dir
}

#' Resolve the template path to use for options commands.
#'
#' @param template_path *[character]* Optional override for the template path.
#' @return *[character]* Path to the template file.
resolve_template_path <- function(template_path = NULL) {
  path <- if (is.null(template_path)) PATHS$FILE_OPTIONS_TEMPLATE else template_path
  assert_options_template_exists(path)
  path
}

#' Resolve the full path to an options file.
#'
#' @param options_file_name *[character]* Name of the options file, including suffix.
#' @param options_dir *[character]* Directory that stores options files.
#' @param must_exist *[logical]* Whether the resolved file must exist.
#' @return *[character]* Full path to the requested options file.
resolve_options_path <- function(options_file_name, options_dir, must_exist = TRUE) {
  assert(!is.null(options_file_name), "An options file name must be supplied.")
  assert(is.character(options_file_name) && length(options_file_name) == 1)
  assert(is.character(options_dir) && length(options_dir) == 1)

  options_path <- file.path(options_dir, options_file_name)

  if (must_exist && !file.exists(options_path)) {
    cli::cli_abort(cli::format_inline(
      "Options file '{options_file_name}' does not exist under path {.path {options_path}}."
    ))
  }

  options_path
}

#' Read a user options file, returning an empty list for empty YAML documents.
#'
#' @param options_path *[character]* Full path to the options file.
#' @return *[list]* Nested options as read from YAML.
read_user_options <- function(options_path) {
  options <- yaml::read_yaml(options_path)
  if (is.null(options)) list() else options
}

#' Load template metadata shared across options operations.
#'
#' @param template_path *[character]* Path to the options template.
#' @return *[list]* A list containing flattened option definitions and their leaf paths.
load_template_metadata <- function(template_path) {
  template <- read_template(template_path)
  definitions <- flatten_template_options(template)
  leaf_paths <- vapply(definitions, `[[`, character(1), "name")

  list(definitions = definitions, leaf_paths = leaf_paths)
}

#' Validate flattened user options against template definitions.
#'
#' @param flat_options *[list]* Flattened user options keyed by fully qualified name.
#' @param template_defs *[list]* Flattened template definitions.
#' @return *[list]* A list with two elements: `errors` (list of validation errors) and
#'   `redundant` (character vector of unrecognised option names).
validate_flat_options <- function(flat_options, template_defs) {
  errors <- list()

  template_names <- vapply(template_defs, `[[`, character(1), "name")

  for (opt_def in template_defs) {
    opt_name <- opt_def$name
    allow_na <- opt_def$allow_na
    expected_type <- get_expected_type(opt_def)

    if (!(opt_name %in% names(flat_options))) {
      errors[[length(errors) + 1]] <- list(
        type = "missing_option",
        value = NULL,
        opt_def = opt_def,
        message = paste0("Missing option: '", opt_name, "'")
      )
      next
    }

    value <- flat_options[[opt_name]]
    error_message <- validate_option_value(value, expected_type, opt_name, allow_na)
    if (!is.null(error_message)) {
      errors[[length(errors) + 1]] <- list(
        type = "type_mismatch",
        value = value,
        opt_def = opt_def,
        message = error_message
      )
    }
  }

  redundant <- setdiff(names(flat_options), template_names)

  list(errors = errors, redundant = redundant)
}

#' List available options files.
#'
#' @param options_dir *[character]* Directory that stores user options files.
#' @param should_return_verbose_names *[logical]* Whether verbose names should be returned.
#' @return *[character]* Vector of option identifiers.
list_options_files <- function(options_dir, should_return_verbose_names = FALSE) {
  if (!dir.exists(options_dir)) {
    return(character(0))
  }

  files <- list.files(
    path = options_dir,
    pattern = CONST$PATTERNS$YAML_FILES$REGEX,
    full.names = should_return_verbose_names
  )

  if (!should_return_verbose_names) {
    return(files)
  }

  verbosity <- get_verbosity()

  verbose_names <- vapply(files, function(file_name) {
    tryCatch(
      {
        if (verbosity >= 4) {
          cli::cli_inform("Reading the options file {.path {file_name}}")
        }
        yaml::read_yaml(file_name)$general$name
      },
      error = function(cond) {
        if (verbosity >= 2) {
          cli::cli_alert_warning("Failed to read the following options file: {.path {file_name}}")
        }
        NA_character_
      }
    )
  }, character(1))

  verbose_names[!is.na(verbose_names)]
}
