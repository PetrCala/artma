#' @description A helper function that searches for a folder from which relative box imports work. It accepts the path to search.
#' @param input_path *\[character\]* The path to turn into a box importable path.
#' @keywords internal
turn_path_into_box_importable <- function(input_path) {
  box::use(
    artma / const[CONST],
    artma / libs / utils[is_empty]
  )

  if (!is.character(input_path) || is_empty(input_path)) {
    rlang::abort(cli::format_inline("Invalid path: {.path {input_path}}"))
  }

  if (!file.exists(input_path)) {
    rlang::abort(cli::format_inline("File does not exist under path: {.path {input_path}}"))
  }

  path_parts <- vector(mode = "character", length = 0)

  i <- tools::file_path_as_absolute(input_path)
  i <- tools::file_path_sans_ext(i) # Strip the .R extension

  while (i != ".") {
    if (grepl(glue::glue("{CONST$PACKAGE_NAME}$"), i)) {
      break
    }
    removed_part <- Reduce(setdiff, strsplit(c(i, dirname(i)), split = "/", fixed = TRUE))
    path_parts <- c(removed_part, path_parts)

    i <- dirname(i)
  }
  if (i == ".") {
    return(NULL) # This indicates the path could not be found
  }

  # Ensure the resulting import statement starts with '<pkg_name> / ...'
  path_parts <- c(CONST$PACKAGE_NAME, path_parts)

  glue::glue_collapse(path_parts, sep = "/")
}

#' @title Turn path into box import
#' @description Given a path, turn this into a box import statement that can be evaluated through 'eval'. Return that (unevaluated) statement.
#' @param path *\[character\]* The path to convert into the import statement.
#'
#' @usage
#' box_import_statement <- turn_path_into_box_import('./some/path')
#' eval(box_import_statement) # Imports the path
#' @export
turn_path_into_box_import <- function(path) {
  box::use(artma / libs / utils[is_empty])

  if (!is.character(path) || is_empty(path)) {
    rlang::abort(glue::glue("Invalid path: {path}"))
  }

  # The box path can be a character, or a vector thereof
  importable_box_path <- turn_path_into_box_importable(path)

  if (is.null(importable_box_path)) {
    rlang::abort("Failed to determine a path for box imports.")
  }

  module_name <- base::basename(importable_box_path)
  parse(text = glue::glue("box::use({module_name}={importable_box_path})"))
}
