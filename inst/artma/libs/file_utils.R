#' Create a folder in the working directory if it does not exist yet
#'
#' @param folder_name [character] Name of the folder. Specify in the format
#' "./<name_of_the_folder>/
#' @param require_existence [logical] Only check the existence of the folder.
#'  Raise an error in case the folder does not exist.
#' @export
ensure_folder_existence <- function(folder_name, require_existence = FALSE) {
  if (!dir.exists(folder_name)) {
    if (require_existence) {
      rlang::abort(
        paste("The folder", folder_name, "must exist in the working directory."),
        class = "folder_not_found"
      )
    }
    dir.create(folder_name, recursive = TRUE)
  }
}


#' Input a vector of file names, that should be located in the folder
#' of the main script, and validate that all are indeed present.
#' Print out a status message after the validation.
#'
#' @param files[vector] A vector of strings.
#' @export
validate_files <- function(files) {
  for (file in files) {
    if (!file.exists(file)) {
      rlang::abort(
        paste(
          file,
          "does not exist or could not be located.",
          "Please make sure to include it in the working directory."
        ),
        class = "file_not_found"
      )
    }
  }
  print("All necessary files located successfully.")
}


#' Create a text file in the specified path with the given message list
#'
#' @param msg_list [list] A list of messages to write to the file
#' @param full_path [character] The full path to the file to create
#' @return [NULL] The function writes the file and does not return anything
#' @export
write_txt_file <- function(msg_list, full_path) {
  box::use(
    artma / libs / utils[is_empty]
  )

  if (is_empty(msg_list)) {
    return(NULL) # Nothing to write
  }
  writeLines(unlist(msg_list), full_path)
}
