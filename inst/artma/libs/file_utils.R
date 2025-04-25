#' Create a folder in the working directory if it does not exist yet
#'
#' @param folder_name *\[character\]* Name of the folder. Specify in the format
#' "./<name_of_the_folder>/
#' @param require_existence *\[logical\]* Only check the existence of the folder.
#'  Raise an error in case the folder does not exist.
#' @export
ensure_folder_existence <- function(folder_name, require_existence = FALSE) {
  box::use(artma / libs / validation[validate])
  validate(is.character(folder_name), is.logical(require_existence))

  if (!dir.exists(folder_name)) {
    if (require_existence) {
      cli::cli_abort(
        paste("The folder", folder_name, "must exist in the working directory."),
        class = "folder_not_found"
      )
    }
    if (getOption("artma.verbose", 3) >= 4) {
      cli::cli_inform("Creating folder {.path {folder_name}}.")
    }
    dir.create(folder_name, recursive = TRUE)
  }
}


#' Input a vector of file names, that should be located in the folder
#' of the main script, and validate that all are indeed present.
#' Print out a status message after the validation.
#'
#' @param files*\[vector\]* A vector of strings.
#' @export
validate_files <- function(files) {
  for (file in files) {
    if (!file.exists(file)) {
      cli::cli_abort(
        paste(
          file,
          "does not exist or could not be located.",
          "Please make sure to include it in the working directory."
        ),
        class = "file_not_found"
      )
    }
  }
  if (getOption("artma.verbose", 3) >= 4) {
    cli::cli_inform("All necessary files located successfully.")
  }
}


#' Create a text file in the specified path with the given message list
#'
#' @param msg_list [list] A list of messages to write to the file
#' @param full_path *\[character\]* The full path to the file to create
#' @return `NULL` The function writes the file and does not return anything
#' @export
write_txt_file <- function(msg_list, full_path) {
  if (rlang::is_empty(msg_list)) {
    return(NULL) # Nothing to write
  }
  writeLines(unlist(msg_list), full_path)
}
