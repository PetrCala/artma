locate_package_root <- function(package_name, start = getwd()) {
  current <- tools::file_path_as_absolute(start)

  repeat {
    desc <- file.path(current, "DESCRIPTION")
    if (file.exists(desc)) {
      pkg_record <- tryCatch(
        base::read.dcf(desc, fields = "Package"),
        error = function(...) NULL
      )
      if (!is.null(pkg_record) && identical(pkg_record[1, 1], package_name)) {
        return(current)
      }
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }

    current <- parent
  }
}

#' @description A helper function that searches for a folder from which relative box imports work. It accepts the path to search.
#' @param input_path *\[character\]* The path to turn into a box importable path.
turn_path_into_box_importable <- function(input_path) {
  box::use(
    artma / const[CONST]
  )

  if (!rlang::is_scalar_character(input_path)) {
    cli::cli_abort(cli::format_inline("Invalid path: {.val {input_path}}"))
  }

  original_input <- input_path
  resolved_path <- input_path

  if (!file.exists(resolved_path)) {
    pkg_root <- locate_package_root(CONST$PACKAGE_NAME)
    if (!is.null(pkg_root)) {
      candidate <- file.path(pkg_root, resolved_path)
      if (file.exists(candidate)) {
        resolved_path <- candidate
      }
    }
  }

  if (!file.exists(resolved_path)) {
    cli::cli_abort(cli::format_inline("File does not exist under path: {.path {original_input}}"))
  }

  path_parts <- vector(mode = "character", length = 0)

  i <- tools::file_path_as_absolute(resolved_path)
  i <- tools::file_path_sans_ext(i) # Strip the .R extension

  while (i != ".") {
    if (grepl(glue::glue("{CONST$PACKAGE_NAME}$"), i)) {
      break
    }
    parent <- dirname(i)
    if (identical(parent, i)) {
      return(NULL) # Reached filesystem root without locating the package
    }
    removed_part <- Reduce(setdiff, strsplit(c(i, parent), split = "/", fixed = TRUE))
    path_parts <- c(removed_part, path_parts)

    i <- parent
  }
  if (i == ".") {
    return(NULL)
  } # This indicates the path could not be found

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
turn_path_into_box_import <- function(path) {
  if (!is.character(path) || rlang::is_empty(path)) {
    cli::cli_abort(glue::glue("Invalid path: {path}"))
  }

  # The box path can be a character, or a vector thereof
  importable_box_path <- turn_path_into_box_importable(path)

  if (is.null(importable_box_path)) {
    cli::cli_abort("Failed to determine a path for box imports.")
  }

  module_name <- base::basename(importable_box_path)
  parse(text = glue::glue("box::use({module_name}={importable_box_path})"))
}

box::export(
  turn_path_into_box_import,
  turn_path_into_box_importable
)
