#' Create a folder in the working directory if it does not exist yet
#'
#' @param folder_name *\[character\]* Name of the folder. Specify in the format
#' "./<name_of_the_folder>/
#' @param require_existence *\[logical\]* Only check the existence of the folder.
#'  Raise an error in case the folder does not exist.
#' @export
ensure_folder_existence <- function(folder_name, require_existence = FALSE) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity]
  )
  validate(is.character(folder_name), is.logical(require_existence))

  if (!dir.exists(folder_name)) {
    if (require_existence) {
      cli::cli_abort(
        paste("The folder", folder_name, "must exist in the working directory."),
        class = "folder_not_found"
      )
    }
    if (get_verbosity() >= 4) {
      cli::cli_inform("Creating folder {.path {folder_name}}.")
    }
    dir.create(folder_name, recursive = TRUE)
  }
}