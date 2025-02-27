#' @title Create user options
#' @description Create a new user options file from an options template.
#' @param options_file_name [character] Name of the new user options file, including the suffix.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param template_path [character, optional] Full path to the options template file.
#' @param args Command line arguments to pass to the template parsing operation. This allows overwriting of the template with custom option values.
#' @returns NULL Creates the options file.
#' @export
options.create <- function(
    options_file_name,
    options_dir = NULL,
    template_path = NULL,
    args = commandArgs(trailingOnly = TRUE)) {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level

  if (!grepl(".yaml$|.yml$", options_file_name)) {
    rlang::abort(glue::glue("Please pass the options file name with the .yaml suffix."))
  }

  box::use(
    artma / paths[PATHS],
    artma / options / index[parse_options_from_template, create_user_options_file]
  )

  if (is.null(options_dir)) options_dir <- PATHS$DIR_USER_OPTIONS
  options_file_path <- file.path(options_dir, options_file_name)

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  parsed_options <- parse_options_from_template(path = template_path, args = args)

  create_user_options_file(
    options_file_name = options_file_name,
    parsed_options = parsed_options,
    path = options_file_path
  )
}

#' @title List available user options
#' @description Retrieves the list of the existing options files and returns their names as a character vector. By default, this retrieves the names of the files including the yaml suffix, but can be modified to retrieve options verbose names instead.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param should_read_verbose_names [logical, optional] If set to TRUE, the custom names of each of the options files are read and returned instead of file names. Defaults to FALSE.
#' @returns vector[character] A character vector with the names of the options available.
#' @export
options.list <- function(options_dir = NULL, should_read_verbose_names = FALSE) {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level
  box::use(
    artma / paths[PATHS],
    artma / const[CONST]
  )

  if (is.null(options_dir)) options_dir <- PATHS$DIR_USER_OPTIONS

  if (!dir.exists(options_dir)) {
    rlang::abort(glue::glue("The following options directory does not exist: {options_dir}"))
  }

  options_files <- list.files(
    path = options_dir,
    pattern = CONST$REGEX$OPTIONS_FILE_SUFFIX,
    # If we are not going to read the file, full names are unnecessary
    full.names = should_read_verbose_names
  )

  options_names <- vector(mode = "character")
  for (file_name in options_files) {
    options_name <- if (should_read_verbose_names) {
      tryCatch(
        {
          logger::log_debug(glue::glue("Reading the options file '{file_name}'"))
          options_name <- yaml::read_yaml(file_name)$general$name
        },
        error = function(cond) {
          logger::log_warn(glue::glue("Failed to read the following options file: {file}"))
        }
      )
    } else {
      file_name
    }
    options_names <- append(options_names, options_name)
  }
  return(options_names)
}

# Copy an existing user options file
# options.copy

# Delete an existing user options file
# options.delete

# Inspect an existing user options file
# options.inspect

# Validate an existing user options file
# options.validate
