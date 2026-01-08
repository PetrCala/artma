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


#' @title Detect a suitable editor
#' @description Attempts to find a suitable editor command by inspecting environment
#'   variables and OS-specific fallbacks.
#' @param sysname *[character, optional]* System name (used for testing). Defaults to `Sys.info()[['sysname']]`.
#' @return *[list]* A list with fields `cmd` and `source` in {"env","auto","none"}.
#' @keywords internal
detect_editor <- function(sysname = NULL) {
  if (is.null(sysname)) {
    sysname <- Sys.info()[["sysname"]]
  }
  if (is.null(sysname)) {
    sysname <- ""
  }
  sysname <- tolower(sysname)

  # 1) Environment variables are the user's explicit preference.
  env_visual <- Sys.getenv("VISUAL", unset = "")
  if (nzchar(env_visual) && editor_available(env_visual)) {
    return(list(cmd = env_visual, source = "env"))
  }

  env_editor <- Sys.getenv("EDITOR", unset = "")
  if (nzchar(env_editor) && editor_available(env_editor)) {
    return(list(cmd = env_editor, source = "env"))
  }

  # 2) OS-specific fallbacks.
  candidates <- character(0)

  if (sysname == "darwin") {
    # IDE-like editors first, then GUI open, then terminal editors.
    candidates <- c(
      "cursor -w",
      "code -w",
      "open -t",
      "nvim",
      "vim",
      "nano",
      "vi"
    )
  } else if (sysname == "windows") {
    candidates <- c("notepad")
  } else {
    # Assume Linux/Unix.
    candidates <- c(
      "nvim",
      "vim",
      "nano",
      "vi",
      "xdg-open"
    )
  }

  for (cmd in candidates) {
    if (editor_available(cmd)) {
      return(list(cmd = cmd, source = "auto"))
    }
  }

  list(cmd = NA_character_, source = "none")
}


#' @title Resolve preferred CLI editor
#' @description Resolves the editor command in this order:
#'   1) `cli.editor` stored in the selected options file (if provided)\n
#'   2) session option `artma.cli.editor`\n
#'   3) environment variables `VISUAL` / `EDITOR`\n
#'   4) OS-specific fallbacks\n
#' @param options_file_path *[character, optional]* Full path to a user options YAML.
#' @return *[list]* A list with fields `cmd` (character scalar or NA) and `source`
#'   in {"options_file","session","env","auto","none"}.
#' @keywords internal
resolve_cli_editor <- function(options_file_path = NULL) {
  # 1) Look into the options file (if provided).
  if (!is.null(options_file_path) && is.character(options_file_path) && length(options_file_path) == 1) {
    if (file.exists(options_file_path)) {
      candidate <- tryCatch(
        {
          opt <- yaml::read_yaml(options_file_path)
          if (is.null(opt$cli$editor)) NA_character_ else opt$cli$editor
        },
        error = function(...) NA_character_
      )

      if (is.character(candidate) && length(candidate) == 1 && !is.na(candidate)) {
        candidate <- trimws(candidate)
        if (nzchar(candidate) && editor_available(candidate)) {
          return(list(cmd = candidate, source = "options_file"))
        }
      }
    }
  }

  # 2) Session option (set after auto-detection to avoid re-detecting).
  session_editor <- getOption("artma.cli.editor", default = NA_character_)
  if (is.character(session_editor) && length(session_editor) == 1 && !is.na(session_editor)) {
    session_editor <- trimws(session_editor)
    if (nzchar(session_editor) && editor_available(session_editor)) {
      return(list(cmd = session_editor, source = "session"))
    }
  }

  # 3/4) Environment + OS fallbacks.
  detected <- detect_editor()
  detected
}


box::export(
  detect_editor,
  editor_available,
  extract_editor_exe,
  resolve_cli_editor
)
