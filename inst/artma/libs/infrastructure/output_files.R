# Recording of files written to disk while a cached workflow runs.
#
# Runtime methods write their graphics during execution, not from the value
# they return, so a cache hit skips those writes. That is fine as long as the
# files are still where the previous run left them; it is a silent regression
# when they are not (the results directory was cleared, the graphics were moved
# away, an export failed halfway).
#
# Writers call `record_output_file()`; `cache_cli()` brackets each cold run with
# `begin_output_file_capture()` / `end_output_file_capture()` and stores the
# recorded paths in the cached artifact. On a hit it re-checks them and falls
# back to recomputation when any have gone missing.

.capture <- new.env(parent = emptyenv())
.capture$frames <- list()

#' @title Record a file written by the current workflow
#' @description Note a path as an output of every capture currently in
#'   progress. A no-op when nothing is capturing, so writers can call it
#'   unconditionally.
#' @param path *\[character\]* Path of the file that was written.
#' @return `NULL`, invisibly.
record_output_file <- function(path) {
  if (length(.capture$frames) == 0L) {
    return(invisible(NULL))
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  normalized <- tryCatch(
    normalizePath(path, mustWork = FALSE),
    error = function(err) path
  )

  .capture$frames <- lapply(.capture$frames, function(frame) c(frame, normalized))
  invisible(NULL)
}

#' @title Start recording output files
#' @description Open a capture frame. Frames nest: a recorded path is added to
#'   every open frame, so an outer cached stage sees the files written by the
#'   cached stages it calls.
#' @return *\[integer\]* The identifier of the frame that was opened.
begin_output_file_capture <- function() {
  .capture$frames <- c(.capture$frames, list(character()))
  length(.capture$frames)
}

#' @title Stop recording output files
#' @description Close the capture frame opened by `begin_output_file_capture()`
#'   and return the paths recorded while it was open. Closing a frame also
#'   closes any frames opened after it, which keeps the stack consistent when a
#'   nested workflow aborts. Closing an already-closed frame is a no-op.
#' @param id *\[integer\]* The frame identifier to close.
#' @return *\[character\]* The unique paths recorded while the frame was open.
end_output_file_capture <- function(id) {
  if (!is.numeric(id) || length(id) != 1L || is.na(id) || id > length(.capture$frames)) {
    return(character())
  }

  files <- .capture$frames[[id]]
  .capture$frames <- .capture$frames[seq_len(id - 1L)]
  unique(files)
}

#' @title Test whether recorded output files are still on disk
#' @description Report whether any of the paths a cached run recorded have since
#'   disappeared. An empty or absent record means nothing to verify, which
#'   counts as intact.
#' @param files *\[character\]* Paths recorded on a previous run.
#' @return *\[logical\]* `TRUE` when at least one recorded file is missing.
recorded_output_files_missing <- function(files) {
  if (!is.character(files) || length(files) == 0L) {
    return(FALSE)
  }
  any(!file.exists(files))
}

box::export(
  begin_output_file_capture,
  end_output_file_capture,
  record_output_file,
  recorded_output_files_missing
)
