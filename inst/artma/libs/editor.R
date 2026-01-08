#' @title Extract executable from editor command
#' @description Returns the executable token from an editor command string.
#'   Used for availability checks.
#' @param cmd *[character]* Editor command, e.g. "code -w" or "/usr/bin/vim".
#' @return *[character|NULL]* The executable token, or `NULL` if it cannot be extracted.
#' @keywords internal
extract_editor_exe <- function(cmd) {
  if (!is.character(cmd) || length(cmd) != 1) {
    return(NULL)
  }

  cmd <- trimws(cmd)
  if (!nzchar(cmd)) {
    return(NULL)
  }

  # Best-effort support for quoted executable paths, e.g. "\"/path with space/vim\" -g"
  if (startsWith(cmd, "\"") || startsWith(cmd, "'")) {
    quote <- substr(cmd, 1, 1)
    rest <- substr(cmd, 2, nchar(cmd))
    end <- regexpr(quote, rest, fixed = TRUE)[1]
    if (!is.na(end) && end > 0) {
      exe <- substr(rest, 1, end - 1)
      exe <- trimws(exe)
      if (nzchar(exe)) {
        return(exe)
      }
    }
  }

  # Unquoted executable: take first whitespace-delimited token
  strsplit(cmd, "\\s+")[[1]][1]
}


#' @title Check whether an editor command is available
#' @description Returns `TRUE` if the command appears runnable on this system.
#' @param cmd *[character]* Editor command
#' @return *[logical]* Whether the editor command is available.
#' @keywords internal
editor_available <- function(cmd) {
  exe <- extract_editor_exe(cmd)
  if (is.null(exe) || !nzchar(exe)) {
    return(FALSE)
  }

  # If an absolute path is provided, check executability directly.
  is_abs_path <- grepl("^/", exe) || grepl("^[A-Za-z]:[/\\\\]", exe) || startsWith(exe, "\\\\")
  if (is_abs_path) {
    if (!file.exists(exe)) {
      return(FALSE)
    }
    if (identical(.Platform$OS.type, "windows")) {
      return(TRUE)
    }
    return(file.access(exe, 1) == 0)
  }

  nzchar(Sys.which(exe))
}


#' @title Ensure system paths are in PATH
#' @description Adds common system paths to PATH for this R session if
#'   they're not already present. This ensures system commands like `open`,
#'   `xdg-open`, etc. can be found even in restricted environments (IDEs,
#'   Docker, SSH).
#' @return NULL (modifies PATH via Sys.setenv)
#' @keywords internal
ensure_system_paths_in_path <- function() {
  current_path <- Sys.getenv("PATH")

  # Common system paths that should be in PATH
  system_paths <- if (.Platform$OS.type == "windows") {
    c("C:\\Windows\\System32", "C:\\Windows")
  } else {
    c("/usr/bin", "/bin", "/usr/local/bin")
  }

  # Check which system paths are missing
  missing_paths <- character(0)
  for (sys_path in system_paths) {
    path_exists <- !grepl(sys_path, current_path, fixed = TRUE)
    if (path_exists && dir.exists(sys_path)) {
      missing_paths <- c(missing_paths, sys_path)
    }
  }

  # Add missing paths to PATH
  if (length(missing_paths) > 0) {
    new_path <- paste(
      c(current_path, missing_paths),
      collapse = .Platform$path.sep
    )
    Sys.setenv(PATH = new_path)
  }

  invisible(NULL)
}


#' @title Detect a suitable editor
#' @description Attempts to find a suitable editor command by checking environment
#'   variables (VISUAL, EDITOR) and falling back to system default file handlers.
#' @return *[list]* A list with fields `cmd` and `source` in {"env","system_default"}.
#' @keywords internal
detect_editor <- function() {
  # Ensure common system paths are in PATH for detection
  # This handles cases where R sessions have restricted PATH (e.g., in some IDEs/environments)
  ensure_system_paths_in_path()

  # 1) Check VISUAL environment variable
  env_visual <- Sys.getenv("VISUAL", unset = "")
  if (nzchar(env_visual) && editor_available(env_visual)) {
    return(list(cmd = env_visual, source = "env"))
  }

  # 2) Check EDITOR environment variable
  env_editor <- Sys.getenv("EDITOR", unset = "")
  if (nzchar(env_editor) && editor_available(env_editor)) {
    return(list(cmd = env_editor, source = "env"))
  }

  # 3) System default file handler
  sysname <- tolower(Sys.info()[["sysname"]])

  # Try OS-specific default first, then fall back to common CLI editors
  candidates <- if (sysname == "darwin") {
    c("open -t", "nano", "vim", "vi")
  } else if (sysname == "windows") {
    c("notepad", "vim", "vi")
  } else {
    c("xdg-open", "nano", "vim", "vi")
  }

  for (cmd in candidates) {
    if (editor_available(cmd)) {
      return(list(cmd = cmd, source = "system_default"))
    }
  }

  # If nothing is available, return the first candidate and let it fail with a clear error
  list(cmd = candidates[1], source = "system_default")
}


#' @title Resolve preferred CLI editor
#' @description Resolves the editor command in this order:
#'   1) `cli.editor` stored in the selected options file (if provided)\n
#'   2) environment variables `VISUAL` / `EDITOR`\n
#'   3) system default file handler (open/notepad/xdg-open)\n
#' @param options_file_path *[character, optional]* Full path to a user options YAML.
#' @return *[list]* A list with fields `cmd` (character scalar) and `source`
#'   in {"options_file","env","system_default"}. Always returns a valid editor command.
#' @keywords internal
resolve_cli_editor <- function(options_file_path = NULL) {
  # 1) Check options file
  if (!is.null(options_file_path) && is.character(options_file_path) &&
    length(options_file_path) == 1 && file.exists(options_file_path)) {
    candidate <- tryCatch(
      {
        opt <- yaml::read_yaml(options_file_path)
        if (is.null(opt$cli$editor)) NA_character_ else opt$cli$editor
      },
      error = function(...) NA_character_
    )

    if (is.character(candidate) && length(candidate) == 1 &&
      !is.na(candidate) && nzchar(trimws(candidate)) &&
      editor_available(trimws(candidate))) {
      return(list(cmd = trimws(candidate), source = "options_file"))
    }
  }

  # 2) Environment variables + system default
  detect_editor()
}


#' @title Build editor function for utils::file.edit
#' @description Creates a function compatible with utils::file.edit's editor option.
#' @param cmd *[character]* Editor command string.
#' @return *[function]* A function that opens files with the specified editor.
#' @keywords internal
build_editor_function <- function(cmd) {
  cmd <- trimws(cmd)
  force(cmd)

  function(file, title) {
    file <- path.expand(file)
    for (path in file) {
      system(paste(cmd, shQuote(path)), wait = TRUE)
    }
    invisible(NULL)
  }
}


#' @title Open file with a CLI editor command
#' @description Opens the file with a CLI editor command (supports arguments).
#' @param path *[character]* File path.
#' @param editor_cmd *[character]* Editor command string.
#' @return *[logical]* TRUE if the command was run.
#' @keywords internal
open_with_cli <- function(path, editor_cmd) {
  if (!is.character(editor_cmd) || length(editor_cmd) != 1 || is.na(editor_cmd)) {
    return(FALSE)
  }
  editor_cmd <- trimws(editor_cmd)
  if (!nzchar(editor_cmd)) {
    return(FALSE)
  }

  editor_fun <- build_editor_function(editor_cmd)
  withr::with_options(
    list(
      editor = editor_fun,
      edit = editor_fun
    ),
    utils::file.edit(path)
  )
  TRUE
}


box::export(
  detect_editor,
  editor_available,
  extract_editor_exe,
  open_with_cli,
  resolve_cli_editor
)
