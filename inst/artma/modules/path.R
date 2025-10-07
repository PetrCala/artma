.is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:\\\\|/)", path)
}

.generate_candidate_paths <- function(path, package_name) {
  normalized <- gsub("\\\\", "/", path, fixed = TRUE)
  normalized <- sub("^\\./", "", normalized)

  variants <- unique(c(
    normalized,
    sub("^inst/", "", normalized),
    sub(paste0("^", package_name, "/"), "", normalized),
    sub(paste0("^inst/", package_name, "/"), "", normalized)
  ))

  variants <- variants[nzchar(variants)]
  variants <- unique(c(normalized, variants, basename(variants)))

  needs_ext <- !grepl("\\.[^/]+$", basename(variants))
  with_ext <- unique(c(variants, paste0(variants[needs_ext], ".R")))
  with_ext[with_ext != ""]
}

.get_path_context <- function(package_name) {
  box::use(artma / paths[PATHS, get_pkg_path])

  base_candidate <- PATHS$PACKAGE_PATH
  if (!dir.exists(base_candidate)) {
    base_candidate <- get_pkg_path()
  }

  base_path <- normalizePath(base_candidate, winslash = "/", mustWork = FALSE)
  pkg_root <- if (basename(base_path) == "inst") dirname(base_path) else base_path
  inst_dir <- file.path(pkg_root, "inst")
  module_root <- file.path(base_path, package_name)

  base_dirs <- unique(Filter(dir.exists, c(base_path, pkg_root, inst_dir, module_root, file.path(module_root, "modules"))))
  list(base_dirs = base_dirs, package_root = pkg_root)
}

.resolve_module_path <- function(input_path, package_name) {
  context <- .get_path_context(package_name)
  candidates <- .generate_candidate_paths(input_path, package_name)

  for (candidate in candidates) {
    candidate_norm <- gsub("\\\\", "/", candidate, fixed = TRUE)
    if (file.exists(candidate_norm)) {
      return(normalizePath(candidate_norm, winslash = "/", mustWork = TRUE))
    }

    if (.is_absolute_path(candidate_norm)) {
      next
    }

    for (base_dir in context$base_dirs) {
      resolved <- file.path(base_dir, candidate_norm)
      if (file.exists(resolved)) {
        return(normalizePath(resolved, winslash = "/", mustWork = TRUE))
      }
    }
  }

  NULL
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
  resolved_path <- .resolve_module_path(input_path, CONST$PACKAGE_NAME)

  if (is.null(resolved_path)) {
    cli::cli_abort(cli::format_inline("File does not exist under path: {.path {original_input}}"))
  }

  path_parts <- character()

  i <- tools::file_path_sans_ext(resolved_path)

  while (i != ".") {
    if (grepl(paste0(CONST$PACKAGE_NAME, "$"), i)) {
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

  paste(path_parts, collapse = "/")
}

#' @title Turn path into box import
#' @description Given a path, turn this into a box import statement that can be evaluated through 'eval'. Return that (unevaluated) statement.
#' @param path *\[character\]* The path to convert into the import statement.
#'
#' @usage
#' box_import_statement <- turn_path_into_box_import('./some/path')
#' eval(box_import_statement) # Imports the path
turn_path_into_box_import <- function(path) {
  if (!rlang::is_scalar_character(path)) {
    cli::cli_abort(cli::format_inline("Invalid path: {.val {path}}"))
  }

  # The box path can be a character, or a vector thereof
  importable_box_path <- turn_path_into_box_importable(path)

  if (is.null(importable_box_path)) {
    cli::cli_abort("Failed to determine a path for box imports.")
  }

  module_name <- base::basename(importable_box_path)
  parse(text = sprintf("box::use(%s=%s)", module_name, importable_box_path))
}

box::export(
  turn_path_into_box_import,
  turn_path_into_box_importable
)
