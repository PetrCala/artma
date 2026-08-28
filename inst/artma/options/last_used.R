#' @title Persistence of the last used options file
#' @description
#' A small marker file inside the options directory (next to the
#' `.welcome_shown` flag) holding the name of the options file the user last
#' ran on. Session entry reads it to resume on that file; every successful
#' bind rewrites it, and deleting the file it names clears it. Every helper
#' tolerates a missing or unreadable marker: reading returns `NULL` and write
#' or clear failures are swallowed, so the persistence can never break a
#' session.
NULL

box::use(
  artma / libs / core / file[ensure_folder_existence],
  artma / libs / core / validation[validate],
  artma / options / files[resolve_options_dir]
)

# Hidden and non-YAML, so the options-file listings never pick it up.
MARKER_FILE_NAME <- ".last_options_file"

#' @title Path of the last-used marker file
#' @param options_dir *\[character, optional\]* Directory holding the options
#'   files. `NULL` uses the package default.
#' @return *\[character\]* The marker file's path.
last_used_marker_path <- function(options_dir = NULL) {
  file.path(resolve_options_dir(options_dir, must_exist = FALSE), MARKER_FILE_NAME)
}

#' @title Read the last used options file name
#' @param options_dir *\[character, optional\]* Directory holding the options
#'   files. `NULL` uses the package default.
#' @return *\[character, optional\]* The remembered file name, or `NULL` when
#'   the marker is missing, empty, or unreadable.
read_last_used_file <- function(options_dir = NULL) {
  tryCatch(
    {
      marker <- last_used_marker_path(options_dir)
      if (!file.exists(marker)) {
        NULL
      } else {
        content <- readLines(marker, n = 1L, warn = FALSE)
        name <- if (length(content) >= 1L) trimws(content[[1L]]) else ""
        if (nzchar(name)) name else NULL
      }
    },
    error = function(e) NULL
  )
}

#' @title Remember an options file as the last used one
#' @param file_name *\[character\]* Name of the options file, including the
#'   suffix.
#' @param options_dir *\[character, optional\]* Directory holding the options
#'   files. `NULL` uses the package default.
#' @return `NULL`, invisibly.
write_last_used_file <- function(file_name, options_dir = NULL) {
  validate(
    is.character(file_name),
    length(file_name) == 1L,
    !is.na(file_name)
  )
  tryCatch(
    {
      resolved_dir <- resolve_options_dir(options_dir, must_exist = FALSE)
      ensure_folder_existence(resolved_dir)
      writeLines(file_name, file.path(resolved_dir, MARKER_FILE_NAME))
    },
    error = function(e) NULL
  )
  invisible(NULL)
}

#' @title Forget the last used options file
#' @param options_dir *\[character, optional\]* Directory holding the options
#'   files. `NULL` uses the package default.
#' @return `NULL`, invisibly.
clear_last_used_file <- function(options_dir = NULL) {
  tryCatch(
    {
      marker <- last_used_marker_path(options_dir)
      if (file.exists(marker)) {
        file.remove(marker)
      }
    },
    error = function(e) NULL
  )
  invisible(NULL)
}

#' @title Clear the marker when the file it names is gone
#' @description Called after options files were deleted: a marker naming a
#'   file that no longer exists must not be resumed next session, whether or
#'   not that file was the one the current session ran on.
#' @param existing_files *\[character\]* The options files that still exist.
#' @param options_dir *\[character, optional\]* Directory holding the options
#'   files. `NULL` uses the package default.
#' @return `NULL`, invisibly.
prune_last_used_file <- function(existing_files, options_dir = NULL) {
  validate(is.character(existing_files) || length(existing_files) == 0L)
  remembered <- read_last_used_file(options_dir)
  if (!is.null(remembered) && !(remembered %in% existing_files)) {
    clear_last_used_file(options_dir)
  }
  invisible(NULL)
}

box::export(
  clear_last_used_file,
  last_used_marker_path,
  prune_last_used_file,
  read_last_used_file,
  write_last_used_file
)
