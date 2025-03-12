static_setup() # nolint: box_usage_linter.

box::use(opts = artma / options / index)

#' @title List available user options
#' @description Retrieves the list of the existing options files and returns their names as a character vector. By default, this retrieves the names of the files including the yaml suffix, but can be modified to retrieve options verbose names instead.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param should_return_verbose_names [logical, optional] If set to TRUE, the custom names of each of the options files are read and returned instead of file names. Defaults to FALSE.
#' @return [vector, character] A character vector with the names of the options available.
#' @export
options.list <- function(...) opts$list_user_options_files(...)

#' @title Create user options
#' @description Create a new user options file from an options template.
#' @param options_file_name [character] Name of the new user options file, including the suffix.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param template_path [character, optional] Full path to the options template file.
#' @param user_input [list, optional] A named list of user-supplied values for these options. If `NULL` or missing entries exist, the function will prompt the user via `readline()` (for required entries) or use defaults (for optional ones).
#' @param should_validate [logical, optional] If TRUE, validate the new options file against the template. Defaults to TRUE.
#' @param should_overwrite [logical, optional] If TRUE, overwrite the file if it already exists. Defaults to FALSE, in which case the user is prompted to confirm the overwrite.
#' @param action_name [character, optional] A name for the action being performed. This is used for logging purposes. Defaults to "create".
#' @return [character] Name of the newly created user options file as a character.
#' @export
options.create <- function(...) opts$create_user_options_file(...)

#' @title Print default user options directory
#' @description Prints the full path to the directory where user options are stored by default
#' @export
options.print_default_dir <- function(...) { # nolint: object_name_linter.
  box::use(artma / paths[PATHS])

  cli::cli_text("By default, user option files are being stored under the following path:")
  cat(cli::style_italic(PATHS$DIR_USER_OPTIONS), sep = "\n")
}

#' @title Copy user options
#' @description Provide a name of a user options file to copy from, and a name of a file to copy to, and copy from the 'from' file to the 'to' file.
#' @param options_file_name_from [character, optional] Name of the options file to copy from. If not provided, the user will be prompted. Defaults to NULL.
#' @param options_file_name_to [character, optional] Name of the options file to copy to. If not provided, the user will be prompted. Defaults to NULL.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @return [NULL]
#' @export
options.copy <- function(...) opts$copy_user_options_file(...)

#' @title Delete user options
#' @description Provide a name of a user options file to delete, and delete that file.
#' @param options_file_name [character, optional] Name of the options file to delete. If not provided, the user will be prompted. Defaults to NULL.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param skip_confirmation [boolean, optional] If passed as TRUE, the user will not be prompted for deletion confirmation. Defaults to FALSE.
#' @return [NULL]
#' @export
options.delete <- function(...) opts$delete_user_options_file(...)

#' Validate a user options file against an options template.
#'
#' This function reads a YAML template and an options file, flattens both structures,
#' and then checks that:
#'  - Every option defined in the template is present in the options file.
#'  - The value for each option is of the correct type.
#'  - (Optionally) It warns about extra options in the file that are not defined in the template.
#'
#' For each problem found (missing option or type mismatch), an error message is printed.
#'
#' @param options_file_name [character] Name of the user options file to validate, including the suffix.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param should_flag_redundant [logical, optional] If TRUE, warn the user about any extraneous options (i.e., options not defined in the options template, such as custom options that the user might have added). Defaults to FALSE.
#' @param should_fail [logical, optional] If TRUE, throw an error if any validation errors are found. Defaults to TRUE.
#' @param template_path [character, optional] Full path to the options template file. Defaults to NULL.
#' @param verbose [logical, optional] If TRUE, print additional information about the validation process. Defaults to TRUE.
#' @return [list] Invisibly returns a list of error messages (empty if no errors).
#' @export
options.validate <- function(...) opts$validate_user_options_file(...)

# Inspect an existing user options file
# options.inspect

#' @title Modify User Options
#' @description Modify an existing user options file with new values.
#'
#' @param options_file_name [character] Name of the user options file to modify, including the suffix.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param template_path [character, optional] Full path to the options template file. Defaults to NULL.
#' @param user_input [list, optional] A named list of user-supplied values for these options. If `NULL` or missing entries exist, the function will prompt the user via `readline()` (for required entries) or use defaults (for optional ones).
#' @param should_validate [logical, optional] If TRUE, validate the modified options file against the template. Defaults to TRUE.
#' @export
options.modify <- function(...) opts$modify_user_options_file(...)

#' @title Options Help
#' @description
#' Prints information for each requested option (or all options if \code{options} is \code{NULL}).
#'
#' @param options [character|NULL] A single option name (dot-separated) or a
#'   character vector thereof. If \code{NULL}, prints **all** options from
#'   the template.
#' @param template_path [character|NULL] Path to the template YAML file.
#'   Defaults to \code{PATHS$FILE_OPTIONS_TEMPLATE}.
#' @return Invisibly returns \code{NULL}, printing the requested information
#'   to the console.
#' @export
options.help <- function(...) opts$options_help(...)

#' @title Fix user options file
#' @description Fix an existing user options file by replacing the values of the options with new ones.
#' @param options_file_name [character] Name of the user options file to fix, including the suffix.
#' @param options_dir [character, optional] Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to NULL.
#' @param template_path [character, optional] Full path to the options template file. Defaults to NULL.
#' @param force_default_overwrites [logical, optional] If TRUE, overwrite invalid option values with the default values from the template. Defaults to FALSE.
#' @return [NULL]
#' @export
options.fix <- function(...) opts$fix_user_options_file(...)

# Copy an existing folder of options into another place
# options.migrate
