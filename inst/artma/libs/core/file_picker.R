#' @title Escape a string for embedding in an AppleScript literal
#' @param x *\[character(1)\]* The raw string.
#' @return *\[character(1)\]* The string, quoted and escaped.
#' @keywords internal
as_applescript_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  paste0("\"", x, "\"")
}

#' @title Pick the backend used for interactive path selection
#' @description Decides which graphical picker, if any, can be opened from this
#'   session. The Tcl/Tk chooser is deliberately never used on macOS: loading
#'   the `tcltk` namespace initializes Aqua Tk inside the R process, and an Aqua
#'   Tk built against a newer AppKit than the running R front-end sends
#'   `-[NSApplication macOSVersion]` to a plain `NSApplication`. The resulting
#'   `NSInvalidArgumentException` is an Objective-C abort, not an R condition:
#'   it kills the whole session (taking the half-written options file with it)
#'   and `tryCatch()` cannot intercept it. macOS therefore uses the native
#'   `osascript` chooser, which runs in its own process.
#' @param os_type *\[character(1)\]* Override for `.Platform$OS.type`.
#' @param sysname *\[character(1)\]* Override for `Sys.info()[["sysname"]]`.
#' @param is_interactive *\[logical(1)\]* Override for `interactive()`.
#' @return *\[character(1)\]* One of `"windows"`, `"macos"`, `"tcltk"`, `"none"`.
#' @export
path_picker_backend <- function(
  os_type = .Platform$OS.type,
  sysname = Sys.info()[["sysname"]],
  is_interactive = interactive()
) {
  if (!isTRUE(is_interactive)) {
    return("none")
  }
  if (identical(os_type, "windows")) {
    return("windows")
  }
  if (identical(sysname, "Darwin")) {
    return(if (nzchar(Sys.which("osascript"))) "macos" else "none")
  }
  tcltk_ok <- isTRUE(unname(capabilities("tcltk"))) &&
    isTRUE(unname(capabilities("X11"))) &&
    !identical(Sys.getenv("DISPLAY"), "") &&
    isTRUE(tryCatch(requireNamespace("tcltk", quietly = TRUE), error = function(e) FALSE))
  if (tcltk_ok) "tcltk" else "none"
}

#' @title Check whether a graphical path picker can be opened
#' @param ... Passed to [path_picker_backend()].
#' @return *\[logical(1)\]* `TRUE` when a picker is available.
#' @export
path_picker_available <- function(...) {
  !identical(path_picker_backend(...), "none")
}

#' @title Build the AppleScript used by the native macOS chooser
#' @param type *\[character(1)\]* Either `"file"` or `"directory"`.
#' @param caption *\[character(1)\]* Dialog prompt.
#' @param default_dir *\[character(1)\]* Directory the dialog opens in.
#' @return *\[character(1)\]* The AppleScript source.
#' @keywords internal
#' @export
build_choose_script <- function(type, caption, default_dir) {
  chooser <- if (identical(type, "file")) "choose file" else "choose folder"
  location <- if (nzchar(default_dir) && dir.exists(default_dir)) {
    paste0(" default location POSIX file ", as_applescript_string(normalizePath(default_dir)))
  } else {
    ""
  }
  paste(
    "try",
    sprintf("  set chosen to (%s with prompt %s%s)", chooser, as_applescript_string(caption), location),
    "  return POSIX path of chosen",
    # -128 is "user cancelled": an empty answer, not a failure.
    "on error number -128",
    "  return \"\"",
    "end try",
    sep = "\n"
  )
}

#' @title Read a path out of `osascript` output
#' @description The chooser prints the selected path on stdout. A non-zero exit
#'   status means the dialog could not be shown at all (no window server, for
#'   instance); both that and a cancelled dialog become `""`.
#' @param out *\[character\]* The `system2()` result.
#' @return *\[character(1)\]* The selected path, or `""`.
#' @keywords internal
#' @export
parse_osascript_output <- function(out) {
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    return("")
  }
  out <- trimws(out)
  out <- out[!is.na(out) & nzchar(out)]
  if (length(out) == 0) "" else out[[length(out)]]
}

#' @title Choose a file or directory with the native macOS chooser
#' @param type *\[character(1)\]* Either `"file"` or `"directory"`.
#' @param caption *\[character(1)\]* Dialog prompt.
#' @param default_dir *\[character(1)\]* Directory the dialog opens in.
#' @return *\[character(1)\]* The selected path, or `""` when cancelled or when
#'   no window server is reachable (a headless SSH session, for instance).
#' @keywords internal
choose_path_macos <- function(type, caption, default_dir) {
  script <- build_choose_script(type, caption, default_dir)
  failed <- character(0)
  attr(failed, "status") <- 1L
  out <- tryCatch(
    suppressWarnings(system2("osascript", args = c("-e", shQuote(script)), stdout = TRUE, stderr = FALSE)),
    error = function(e) failed
  )
  parse_osascript_output(out)
}

#' @title Choose a file or directory interactively
#' @description Opens the platform's graphical picker and returns the selected
#'   path. Cancelling the dialog, or running where no picker is available,
#'   returns `""` rather than a zero-length vector or `NA`, so callers can test
#'   the result with `nzchar()` alone.
#' @param type *\[character(1)\]* Either `"file"` or `"directory"`.
#' @param caption *\[character(1)\]* Dialog prompt.
#' @param default_dir *\[character(1)\]* Directory the dialog opens in.
#' @return *\[character(1)\]* The selected path, or `""`.
#' @export
choose_path_interactively <- function(
  type = c("file", "directory"),
  caption = NULL,
  default_dir = getwd()
) {
  box::use(artma / libs / core / validation[assert])

  type <- match.arg(type)
  if (is.null(caption)) {
    caption <- if (identical(type, "file")) "Select a file" else "Select a directory"
  }
  backend <- path_picker_backend()
  assert(!identical(backend, "none"), "No graphical path picker is available in this session.")

  selected <- switch(backend,
    macos = choose_path_macos(type, caption, default_dir),
    windows = {
      if (identical(type, "file")) {
        picker <- utils::getFromNamespace("choose.files", "utils")
        picker(default = "", caption = caption, multi = FALSE)
      } else {
        picker <- utils::getFromNamespace("choose.dir", "utils")
        picker(default = default_dir, caption = caption)
      }
    },
    tcltk = {
      selected <- if (identical(type, "file")) {
        tcltk::tk_choose.files(default = "", caption = caption, multi = FALSE)
      } else {
        tcltk::tk_choose.dir(default = default_dir, caption = caption)
      }
      Sys.sleep(0.5) # Allow tk to print the closing message into the console
      selected
    }
  )

  # Every backend has its own way of saying "cancelled": character(0), NA, or "".
  if (length(selected) != 1 || is.na(selected)) {
    return("")
  }
  selected
}
