static_setup() # nolint: box_usage_linter. # Imported on a package-level

box::use(opts = artma / options / index)


#' @title List available user options
#' @description Retrieves the list of the existing options files and returns their names as a character vector. By default, this retrieves the names of the files including the yaml suffix, but can be modified to retrieve options verbose names instead.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param should_return_verbose_names [logical, optional] If set to TRUE, the custom names of each of the options files are read and returned instead of file names. Defaults to FALSE.
#' @returns vector[character] A character vector with the names of the options available.
#' @export
options.list <- function(options_dir = NULL, should_return_verbose_names = FALSE) {
  opts$list_user_options_files(
    options_dir = options_dir,
    should_return_verbose_names = should_return_verbose_names
  )
}


#' @title Create user options
#' @description Create a new user options file from an options template.
#' @param options_file_name [character] Name of the new user options file, including the suffix.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param template_path [character, optional] Full path to the options template file.
#' @returns [character] Name of the newly created user options file.
#' @export
options.create <- function(
    options_file_name = NULL,
    options_dir = NULL,
    template_path = NULL) {
  opts$create_user_options_file(
    options_file_name = options_file_name,
    options_dir = options_dir,
    template_path = template_path
  )
}

#' @title Print default user options directory
#' @description Prints the full path to the directory where user options are stored by default
#' @export
options.print_default_dir <- function() { # nolint: object_name_linter.
  box::use(artma / paths[PATHS])

  cli::cli_text("By default, user option files are being stored under the following path:")
  cat(cli::style_italic(PATHS$DIR_USER_OPTIONS), sep = "\n")
}

#' @title Copy user options
#' @description Provide a name of a user options file to copy from, and a name of a file to copy to, and copy from the 'from' file to the 'to' file.
#' @param options_file_name_from [character, optional] Name of the options file to copy from. If not provided, the user will be prompted. Defaults to NULL.
#' @param options_file_name_to [character, optional] Name of the options file to copy to. If not provided, the user will be prompted. Defaults to NULL.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @return NULL
#' @export
options.copy <- function(
    options_file_name_from = NULL,
    options_file_name_to = NULL,
    options_dir = NULL) {
  opts$copy_user_options_file(
    options_file_name_from = options_file_name_from,
    options_file_name_to = options_file_name_to,
    options_dir = options_dir
  )
}


# Delete an existing user options file
# options.delete

# Inspect an existing user options file
# options.inspect

# Validate an existing user options file
# options.validate

# Copy an existing folder of options into another place
# options.migrate
