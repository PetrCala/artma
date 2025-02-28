#' @description A helper function that searches for a folder from which relative box imports work. It accepts the path to search, as well as a box path of type character.
#' @param input_path [character] The path to turn into a box importable path.
#' @param box_path_character [character] A box path provided as a character.
#' @return [character] A box importable path.
#' @keywords internal
turn_path_into_box_importable <- function(input_path, box_path_character) {
  if (!is.character(box_path_character)) {
    rlang::abort(glue::glue("The box path must be passed as a character: {box_path_character}"))
  }
  box::use(artma / const[CONST])

  if (grepl(glue::glue("{CONST$PACKAGE_NAME}$"), box_path_character)) {
    logger::log_debug(glue::glue("The box path {box_path_character} does not end in the package name. Skipping this path..."))
    return(NULL)
  }

  path_parts <- vector(mode = "character", length = 0)

  i <- tools::file_path_as_absolute(input_path)
  i <- tools::file_path_sans_ext(i) # Strip the .R extension

  while (i != ".") {
    if (i == box_path_character) {
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
#' @param path [character] The path to convert into the import statement.
#'
#' @usage
#' box_import_statement <- turn_path_into_box_import('./some/path')
#' eval(box_import_statement) # Imports the path
#' @export
turn_path_into_box_import <- function(path) {
  box::use(artma / libs / utils[is_empty])

  box_path <- getOption("box.path", character(0))

  if (!is.character(path) || is_empty(path)) {
    rlang::abort(glue::glue("Invalid path: {path}"))
  }
  if (is_empty(box_path)) {
    rlang::abort(glue::glue("Invalid box path: {box_path}"))
  }

  # The box path can be a character, or a vector thereof
  importable_box_path <- NULL
  if (is.vector(box_path)) {
    i <- 0
    while (is.null(importable_box_path) && i < length(box_path)) {
      box_path_to_search <- box_path[i + 1] # 1-indexing
      importable_box_path <- turn_path_into_box_importable(path, box_path_to_search)
      i <- i + 1
    }
  } else {
    importable_box_path <- turn_path_into_box_importable(path, box_path)
  }

  if (is.null(importable_box_path)) {
    rlang::abort("Failed to determine a path for box imports.")
  }

  module_name <- base::basename(importable_box_path)
  parse(text = glue::glue("box::use({module_name}={importable_box_path})"))
}
