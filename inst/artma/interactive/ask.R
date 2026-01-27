#' @title Ask for overwrite permission
#' @description Ask the user if they want to overwrite an existing file
#' @param file_path *\[character\]* Path to the file that would be overwritten
#' @param action_name *\[character, optional\]* Name of the action being performed, for the abort message. Defaults to "the operation"
#' @param should_overwrite *\[logical, optional\]* Whether to overwrite without asking. If TRUE, will overwrite without prompting. If FALSE, will abort without prompting. If NULL (default), will prompt interactively.
#' @return *\[logical\]* TRUE if overwrite is permitted, FALSE otherwise
ask_for_overwrite_permission <- function(file_path, action_name = "the operation", should_overwrite = NULL) {
  if (file.exists(file_path)) {
    if (!is.null(should_overwrite)) {
      if (!should_overwrite) {
        cli::cli_abort(sprintf("Aborting %s.", action_name))
      }
      return(TRUE)
    }

    if (!interactive()) {
      cli::cli_abort("Cannot prompt for overwrite permission in non-interactive mode.")
    }

    overwrite_permitted <- climenu::select(
      choices = c("Yes", "No"),
      prompt = cli::format_inline("A file already exists under the path {.path {file_path}}. Do you wish to overwrite the contents of this file?")
    )
    if (overwrite_permitted != "Yes") {
      cli::cli_abort(sprintf("Aborting %s.", action_name))
    }
  }
  TRUE
}

box::export(ask_for_overwrite_permission)
