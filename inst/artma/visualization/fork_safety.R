# Raster graphics devices inside forked workers.
#
# `parallel::mclapply()` runs a method by forking the current process. On macOS
# the Objective-C runtime refuses to run in a child when a class initialiser may
# have been in progress at `fork()` time, and aborts the child instead of
# returning an error. Both `ragg::agg_png()` and the default quartz-backed
# `grDevices::png()` reach that code path through their font handling, so a
# method that exports a plot kills its worker outright. `mclapply()` then reports
# "parallel jobs did not deliver results" and yields `NULL` for that method,
# which is why the failure used to surface with an empty error message.
#
# The cairo-backed `grDevices::png(type = "cairo")` does not touch the
# Objective-C runtime and renders correctly in a fork, so it stands in for the
# faster ragg device for the duration of a forked method run. Output dimensions
# are unchanged; only the rasteriser differs.
#
# That device is not universally usable: some headless macOS installations
# report `capabilities("cairo") == TRUE` yet write no file at all. It is
# therefore probed once per session, and when the probe fails the layer falls
# back to sequential execution rather than losing every method that draws.

box::use(
  artma / libs / core / validation[validate]
)

#' @title Option flagging the current process as a forked method worker
#' @description
#' Set in the child by `execute_method_layer()`. A forked child gets its own
#' copy of the R session, so writing it there cannot leak into the parent.
#' @keywords internal
FORKED_WORKER_OPTION <- "artma.temp.forked_worker" # nolint: object_name_linter.

#' @title Whether forking is hostile to the default graphics devices
#' @description
#' Only macOS aborts the child; on other platforms ragg forks cleanly and is
#' left in place.
#' @param sysname *\[character, optional\]* Platform name. Injectable for
#'   testing; defaults to `Sys.info()[["sysname"]]`.
#' @return *\[logical\]* `TRUE` when the platform's default PNG devices are
#'   unsafe to use in a forked child.
#' @keywords internal
graphics_fork_is_hostile <- function(sysname = NULL) {
  sysname <- sysname %||% Sys.info()[["sysname"]]
  identical(sysname, "Darwin")
}

#' @title Cached result of the cairo probe
#' @keywords internal
cairo_probe <- new.env(parent = emptyenv())

#' @title Whether the cairo device actually writes a PNG here
#' @description
#' `capabilities("cairo")` reports how R was built, which is not the same as the
#' device working: on some headless macOS installations it returns `TRUE` while
#' the device silently produces no file. Since the whole fork-safety strategy
#' rests on this device, verify it by writing a throwaway PNG rather than
#' trusting the capability flag.
#' @return *\[logical\]* `TRUE` when a cairo PNG was written successfully.
#' @keywords internal
probe_cairo_png <- function() {
  if (!isTRUE(capabilities("cairo"))) {
    return(FALSE)
  }

  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)

  opened <- FALSE
  ok <- tryCatch(
    {
      grDevices::png(
        filename = path, width = 200, height = 200, units = "px", res = 72, type = "cairo"
      )
      opened <- TRUE
      graphics::plot.new()
      grDevices::dev.off()
      opened <- FALSE
      TRUE
    },
    error = function(err) FALSE,
    warning = function(cond) FALSE
  )

  if (opened) {
    try(grDevices::dev.off(), silent = TRUE)
  }

  isTRUE(ok) && file.exists(path) && file.info(path)$size > 0L
}

#' @title Whether a fork-safe PNG device is available
#' @description
#' Probes once per session and caches the answer; the probe opens a device, so
#' repeating it per plot would be wasteful.
#' @param refresh *\[logical, optional\]* Re-run the probe instead of reusing the
#'   cached answer. Injectable for testing.
#' @return *\[logical\]* `TRUE` when `grDevices::png(type = "cairo")` can be used.
#' @keywords internal
fork_safe_png_available <- function(refresh = FALSE) {
  if (isTRUE(refresh) || is.null(cairo_probe$ok)) {
    cairo_probe$ok <- probe_cairo_png()
  }
  cairo_probe$ok
}

#' @title Whether the current process is a forked method worker
#' @return *\[logical\]* `TRUE` inside a child spawned by `execute_method_layer()`.
#' @keywords internal
in_forked_worker <- function() {
  isTRUE(getOption(FORKED_WORKER_OPTION, FALSE))
}

#' @title Whether plot exports may run inside a forked worker
#' @description
#' Used by `resolve_worker_count()` to keep a layer sequential when graphics are
#' being exported on a platform that would abort the child and offers no
#' fork-safe device to fall back on.
#' @return *\[logical\]* `TRUE` when a forked worker can safely write a PNG.
#' @keywords internal
graphics_survive_fork <- function() {
  !graphics_fork_is_hostile() || fork_safe_png_available()
}

#' @title Whether to substitute the fork-safe PNG device
#' @return *\[logical\]* `TRUE` when the cairo device should replace ragg/quartz.
#' @keywords internal
use_fork_safe_png_device <- function() {
  in_forked_worker() && graphics_fork_is_hostile() && fork_safe_png_available()
}

#' @title Evaluate an expression with the forked-worker flag set
#' @description
#' Marks the current process as a forked method worker so the graphics helpers
#' pick a fork-safe device. The previous option value is restored on exit, which
#' keeps the helper harmless if it is ever called in the parent.
#' @param expr *\[any\]* Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
with_forked_worker_flag <- function(expr) {
  old <- options(stats::setNames(list(TRUE), FORKED_WORKER_OPTION))
  on.exit(options(old), add = TRUE)
  expr
}

#' @title A cairo-backed PNG device usable as a `ggsave()` device
#' @description
#' `ggplot2::ggsave()` resolves the plot size to inches before calling the
#' device, whereas `grDevices::png()` measures in pixels unless told otherwise,
#' so `units` is pinned to inches here. Without it the device would receive a
#' handful of pixels and write an essentially blank file.
#' @param filename *\[character\]* Output path.
#' @param width *\[numeric\]* Width, in inches.
#' @param height *\[numeric\]* Height, in inches.
#' @param res *\[numeric, optional\]* Resolution in pixels per inch.
#' @param ... Ignored; absorbs arguments `ggsave()` passes to other devices.
#' @return NULL (invisibly). Called for its side effect of opening a device.
#' @keywords internal
fork_safe_png_device <- function(filename, width, height, res = 150, ...) {
  validate(
    is.character(filename),
    is.numeric(width), width > 0,
    is.numeric(height), height > 0,
    is.numeric(res), res > 0
  )

  grDevices::png(
    filename = filename,
    width = width,
    height = height,
    units = "in",
    res = res,
    type = "cairo"
  )

  invisible(NULL)
}

box::export(
  FORKED_WORKER_OPTION,
  fork_safe_png_available,
  fork_safe_png_device,
  graphics_fork_is_hostile,
  graphics_survive_fork,
  in_forked_worker,
  probe_cairo_png,
  use_fork_safe_png_device,
  with_forked_worker_flag
)
