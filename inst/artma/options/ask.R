#' @title Ask for options file name
#' @description Ask the user to input a name of an options file. Clean the user's input and return it as a string.
#' @param should_clean [logical, optional] Whether to clean the input string. Defaults to TRUE
#' @param prompt [character, optional] The prompt to use. Defaults to a generic prompt.
#' @return [character] The options file name.
ask_for_options_file_name <- function(should_clean = TRUE, prompt = NULL) {
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(artma / options / utils[parse_options_file_name])

  prompt <- prompt %||% "Please provide the name for your options file, including the .yaml suffix: "
  options_file_name <- readline(prompt = prompt)

  if (should_clean) {
    options_file_name <- parse_options_file_name(options_file_name)
  }

  return(options_file_name)
}


#' @title Ask for an existing options file name
#' @description Prompt the user to select from an existing list of user options files. Return the name of the selected file as a character.
#' @param options_dir [character, optional] Name of the directory to list the files from. Defaults to NULL.
#' @param prompt [character, optional] The prompt to use when asking for the user options file name. Defaults to NULL.
#' @return [character] Name of the selected file.
ask_for_existing_options_file_name <- function(options_dir = NULL, prompt = NULL) { # nolint: object_length_linter.
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(
    artma / libs / utils[is_empty],
    artma / options / utils[list_user_options_files],
  )

  prompt <- prompt %||% "Please select the user options file name you would like to use."

  user_options_file_names <- list_user_options_files(options_dir = options_dir)
  if (length(user_options_file_names) == 0) {
    stop("No existing user options files were found. Aborting...")
  }

  selected_file_name <- utils::select.list(
    title = prompt,
    choices = user_options_file_names
  )
  if (is_empty(selected_file_name)) {
    stop("No user options file was selected. Aborting...")
  }
  return(selected_file_name)
}

box::export(
  ask_for_options_file_name,
  ask_for_existing_options_file_name
)
