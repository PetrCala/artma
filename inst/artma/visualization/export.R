#' @title Graphics Export Utilities
#' @description
#' Shared utilities for exporting visualizations to files.


#' Ensure export directory exists
#'
#' @param path *\[character\]* Directory path
#' @return *\[character\]* The normalized path (invisibly)
#' @keywords internal
ensure_export_dir <- function(path) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(path), length(path) == 1)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(normalizePath(path, mustWork = FALSE))
}


#' Build export filename
#'
#' @description
#' Constructs a standardized filename for exported plots.
#'
#' @param base_name *\[character\]* Base name (e.g., "box_plot")
#' @param factor_by *\[character\]* Factor variable name
#' @param index *\[integer, optional\]* Plot index for multi-plot exports
#' @param extension *\[character\]* File extension. Defaults to "png".
#'
#' @return *\[character\]* Filename (without directory)
#'
#' @examples
#' \dontrun{
#' build_export_filename("box_plot", "country")
#' # Returns: "box_plot_country.png"
#'
#' build_export_filename("box_plot", "country", index = 2)
#' # Returns: "box_plot_country_2.png"
#' }
build_export_filename <- function(base_name, factor_by, index = NULL, extension = "png") {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.character(base_name),
    is.character(factor_by),
    is.character(extension)
  )

  if (!is.null(index)) {
    validate(is.numeric(index), index > 0)
    filename <- paste0(base_name, "_", factor_by, "_", as.integer(index), ".", extension)
  } else {
    filename <- paste0(base_name, "_", factor_by, ".", extension)
  }

  filename
}


#' Resolution shared by every base-graphics export
#'
#' @description
#' Base-graphics devices previously opened at 70 or 90 dpi while `save_plot()`
#' rendered ggplot figures at 150. Text is sized in points, which is a physical
#' unit, so the same nominal font came out visibly smaller relative to the
#' canvas on a base plot than on a ggplot one and the two read as different
#' house styles inside a single report. Every base-graphics device now opens at
#' this resolution, matching `save_plot()`'s default.
BASE_PLOT_DPI <- 150


#' Device dimensions for a base-graphics canvas given in inches
#'
#' @description
#' Base plots size their margins, legends and labels in physical units, so the
#' canvas is specified in inches and the pixel count follows from the shared
#' resolution. Passing pixels directly makes the physical size an accident of
#' whatever `res` happened to be in force.
#'
#' @param width_in *\[numeric\]* Canvas width, in inches
#' @param height_in *\[numeric\]* Canvas height, in inches
#' @param graph_scale *\[numeric\]* Scale multiplier for the resolution
#'
#' @return *\[list\]* With `width`, `height` (pixels) and `res`
#' @keywords internal
base_plot_device_size <- function(width_in, height_in, graph_scale = 1) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.numeric(width_in), width_in > 0,
    is.numeric(height_in), height_in > 0,
    is.numeric(graph_scale), graph_scale > 0
  )

  res <- BASE_PLOT_DPI * graph_scale
  list(
    width = width_in * res,
    height = height_in * res,
    res = res
  )
}


#' Open a raster graphics device for writing a PNG file
#'
#' @description
#' Opens `ragg::agg_png` when the `ragg` package is available, since it renders
#' substantially faster than the base `grDevices::png` device. Falls back to
#' `grDevices::png` when `ragg` is not installed.
#'
#' Inside a forked method worker on a platform where those devices would abort
#' the child, a cairo-backed device is used instead. See
#' `artma/visualization/fork_safety`.
#'
#' @param path *\[character\]* Full file path (including filename)
#' @param width *\[numeric\]* Device width, in `units`
#' @param height *\[numeric\]* Device height, in `units`
#' @param units *\[character\]* Units for width/height. Defaults to "px".
#' @param res *\[numeric\]* Resolution in pixels per inch. Defaults to
#'   `BASE_PLOT_DPI`.
#' @param record *\[logical\]* Register the file via `record_output_file()`.
#'   Set to FALSE for throwaway preview renders (tempfiles), which must not
#'   enter the cache's output-file capture. Defaults to TRUE.
#'
#' @return NULL (invisibly). Called for its side effect of opening a graphics device.
#' @keywords internal
open_png_device <- function(path, width, height, units = "px", res = BASE_PLOT_DPI, record = TRUE) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / infrastructure / output_files[record_output_file],
    artma / visualization / fork_safety[use_fork_safe_png_device]
  )

  validate(
    is.character(path),
    is.numeric(width), width > 0,
    is.numeric(height), height > 0,
    is.character(units),
    is.numeric(res), res > 0,
    is.logical(record)
  )

  if (use_fork_safe_png_device()) {
    grDevices::png(
      filename = path, width = width, height = height,
      units = units, res = res, type = "cairo"
    )
  } else if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = path, width = width, height = height, units = units, res = res)
  } else {
    grDevices::png(filename = path, width = width, height = height, units = units, res = res)
  }

  # Base-graphics plots (BMA correlation and model-size charts, the non-linear
  # diagnostics) never pass through `save_plot()`, so record them here instead.
  # Without this a cached rerun cannot tell that these files went missing.
  if (record) {
    record_output_file(path)
  }

  invisible(NULL)
}


#' Whether previews should open exported files instead of printing
#'
#' @description
#' In terminal R on macOS the interactive device is quartz, whose windows only
#' process events while R idles at the prompt: they beachball for the whole
#' method run, and successive previews draw into the same window so only the
#' last plot survives a multi-method run. Previews there go through the
#' exported PNG file opened in the system viewer instead, which has its own
#' event loop and keeps each plot separately browsable. GUI front ends
#' (RStudio, Positron, R.app) pump their device's events themselves, so they
#' keep the plain `print()` preview.
#'
#' @param interactive_session *\[logical, optional\]* Injectable for testing;
#'   defaults to `interactive()`.
#' @param sysname *\[character, optional\]* Injectable for testing; defaults to
#'   `Sys.info()[["sysname"]]`.
#' @param gui *\[character, optional\]* Injectable for testing; defaults to
#'   `.Platform$GUI`.
#' @return *\[logical\]* `TRUE` when previews should open files.
use_file_preview <- function(interactive_session = NULL, sysname = NULL, gui = NULL) {
  interactive_session <- interactive_session %||% interactive()
  sysname <- sysname %||% Sys.info()[["sysname"]]
  gui <- gui %||% .Platform$GUI

  # "X11" (or "unused" on builds without X11 support) is what plain terminal R
  # reports on macOS; every GUI front end announces itself with its own value.
  isTRUE(interactive_session) &&
    identical(sysname, "Darwin") &&
    gui %in% c("X11", "unused")
}


#' Whether the current preview must be rendered to a file first
#'
#' @description
#' Call-site companion to `use_file_preview()`: methods use it to decide
#' whether a preview needs an exported PNG even when `export_graphics` is off.
#' A forked worker never previews, so it never needs the file either.
#'
#' @return *\[logical\]* `TRUE` when a preview requires an exported file.
preview_needs_file <- function() {
  box::use(artma / visualization / fork_safety[in_forked_worker])

  !in_forked_worker() && use_file_preview()
}


#' Open an exported plot file in the system viewer without stealing focus
#'
#' @param path *\[character\]* Path of the file to open.
#' @return NULL (invisibly)
#' @keywords internal
open_preview_file <- function(path) {
  box::use(artma / libs / core / utils[get_verbosity])

  status <- suppressWarnings(
    system2("open", c("-g", shQuote(path)), stdout = FALSE, stderr = FALSE)
  )

  if (!identical(status, 0L) && get_verbosity() >= 2) {
    cli::cli_alert_warning("Could not open the plot preview {.file {path}}")
  }

  invisible(NULL)
}


#' Preview a plot on the interactive device or via its exported file
#'
#' @description
#' Methods preview their plots at verbosity >= 3. The default is to print
#' them, which opens the session's default interactive device. Two situations
#' divert from that:
#'
#' - In a forked method worker the device must not be touched: on macOS it is
#'   quartz, whose Objective-C runtime aborts a forked child outright (killing
#'   the method before its file exports run), and on other platforms each
#'   child would pop its own on-screen device. Previews are skipped there.
#' - When `use_file_preview()` says the interactive device would be a
#'   beachballing terminal quartz window, the exported PNG at `path` is opened
#'   in the system viewer instead; with no usable `path` the preview is
#'   skipped rather than printed.
#'
#' @param plot *\[any\]* A printable plot object. `NULL` is skipped when
#'   printing; a file preview only needs `path`.
#' @param path *\[character, optional\]* Path of the exported PNG to open when
#'   file previews are in effect. `NULL` or `NA` means no file is available.
#' @param file_preview *\[logical, optional\]* Injectable for testing; defaults
#'   to `use_file_preview()`.
#' @param opener *\[function, optional\]* Injectable for testing; defaults to
#'   `open_preview_file()`.
#' @return NULL (invisibly)
preview_plot <- function(plot, path = NULL, file_preview = NULL, opener = NULL) {
  box::use(
    artma / libs / core / validation[validate],
    artma / visualization / fork_safety[in_forked_worker]
  )

  validate(is.null(path) || (is.character(path) && length(path) == 1))

  if (in_forked_worker()) {
    return(invisible(NULL))
  }

  file_preview <- file_preview %||% use_file_preview()

  if (isTRUE(file_preview)) {
    if (!is.null(path) && !is.na(path) && file.exists(path)) {
      opener <- opener %||% open_preview_file
      opener(path)
    }
    return(invisible(NULL))
  }

  if (is.null(plot)) {
    return(invisible(NULL))
  }

  suppressWarnings(print(plot)) # nolint: undesirable_function_linter.
  invisible(NULL)
}


#' Save a ggplot2 plot to file
#'
#' @description
#' Wrapper around ggplot2::ggsave with artma defaults.
#'
#' `scale` multiplies the resolution as well as the pixel dimensions. Scaling
#' the dimensions alone enlarges the canvas while text stays a fixed physical
#' size, so at the default `graph_scale` of 2 every exported figure came out
#' with its labels at half their intended size relative to the plot. Scaling
#' both keeps the layout identical and simply renders it at higher resolution.
#'
#' @param plot *\[ggplot\]* The plot to save
#' @param path *\[character\]* Full file path (including filename)
#' @param width *\[numeric\]* Plot width
#' @param height *\[numeric\]* Plot height
#' @param scale *\[numeric\]* Scale multiplier for dimensions and resolution.
#'   Defaults to 1.
#' @param units *\[character\]* Units for width/height. Defaults to "px".
#' @param dpi *\[numeric\]* Base resolution, before `scale`. Defaults to 150.
#' @param record *\[logical\]* Register the file via `record_output_file()` and
#'   allow the optional HTML companion export. Set to FALSE for throwaway
#'   preview renders (tempfiles), which are not run outputs. Defaults to TRUE.
#'
#' @return *\[character\]* The path where the plot was saved (invisibly)
save_plot <- function(plot, path, width = 800, height = 1100, scale = 1, units = "px", dpi = 150,
                      record = TRUE) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / libs / infrastructure / output_files[record_output_file],
    artma / visualization / fork_safety[fork_safe_png_device, use_fork_safe_png_device]
  )

  validate(
    ggplot2::is_ggplot(plot),
    is.character(path),
    is.numeric(width), width > 0,
    is.numeric(height), height > 0,
    is.numeric(scale), scale > 0,
    is.logical(record)
  )

  dir_path <- dirname(path)
  ensure_export_dir(dir_path)

  if (file.exists(path)) {
    file.remove(path)
  }

  device <- if (use_fork_safe_png_device()) {
    fork_safe_png_device
  } else if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png
  } else {
    NULL
  }

  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width * scale,
    height = height * scale,
    units = units,
    dpi = dpi * scale,
    device = device
  )

  # Graphics are a side effect of the method run, not part of its return value,
  # so record them: a cache hit that replays the value must not leave the
  # results directory without its plots. A preview tempfile is not a run
  # output, so it is neither recorded nor given an HTML companion.
  if (record) {
    record_output_file(path)
  }

  if (get_verbosity() >= 4) {
    cli::cli_alert_success("Exported plot to {.file {path}}")
  }

  if (record && isTRUE(getOption("artma.visualization.export_html", FALSE))) {
    html_path <- sub("\\.[^.]*$", ".html", path)
    save_plot_html(plot, html_path)
  }

  invisible(path)
}


#' Save a ggplot2 plot as an interactive HTML widget
#'
#' @description
#' Converts a ggplot2 plot to an interactive plotly widget and saves it as a
#' standalone HTML file via htmlwidgets. Requires the optional `plotly` and
#' `htmlwidgets` packages; if either is unavailable, the export is skipped
#' with a warning instead of aborting.
#'
#' @param plot *\[ggplot\]* The plot to export
#' @param path *\[character\]* Full file path (including filename, e.g. ending in ".html")
#'
#' @return *\[character\]* The path where the widget was saved, or `NULL` (invisibly) if skipped
#' @keywords internal
save_plot_html <- function(plot, path) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / libs / infrastructure / output_files[record_output_file]
  )

  validate(ggplot2::is_ggplot(plot), is.character(path))

  if (!requireNamespace("plotly", quietly = TRUE) || !requireNamespace("htmlwidgets", quietly = TRUE)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "Skipping interactive HTML export: install {.pkg plotly} and {.pkg htmlwidgets} to enable it."
      )
    }
    return(invisible(NULL))
  }

  dir_path <- dirname(path)
  ensure_export_dir(dir_path)

  if (file.exists(path)) {
    file.remove(path)
  }

  widget <- plotly::ggplotly(plot)
  htmlwidgets::saveWidget(widget, file = path, selfcontained = TRUE)

  record_output_file(path)

  if (get_verbosity() >= 4) {
    cli::cli_alert_success("Exported interactive plot to {.file {path}}")
  }

  invisible(path)
}


#' Export a single base-graphics plot to a PNG file
#'
#' @description
#' Opens a PNG device sized `width`/`height` (scaled by `graph_scale`),
#' invokes `draw()` to render into it, and guarantees the device is closed
#' even if `draw()` errors.
#'
#' @param draw *\[function\]* Zero-argument function that draws the plot
#' @param path *\[character\]* Full file path (including filename)
#' @param width *\[numeric\]* Unscaled device width, in pixels at `BASE_PLOT_DPI`
#' @param height *\[numeric\]* Unscaled device height, in pixels at `BASE_PLOT_DPI`
#' @param graph_scale *\[numeric\]* Scale multiplier for dimensions and resolution
#' @param record *\[logical\]* Register the file via `record_output_file()`.
#'   Defaults to TRUE.
#'
#' @return NULL (invisibly)
#' @keywords internal
export_base_plot <- function(draw, path, width, height, graph_scale, record = TRUE) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.function(draw),
    is.character(path),
    is.numeric(width), width > 0,
    is.numeric(height), height > 0,
    is.numeric(graph_scale), graph_scale > 0
  )

  if (file.exists(path)) {
    file.remove(path)
  }

  size <- base_plot_device_size(
    width_in = width / BASE_PLOT_DPI,
    height_in = height / BASE_PLOT_DPI,
    graph_scale = graph_scale
  )
  open_png_device(
    path,
    width = size$width,
    height = size$height,
    units = "px",
    res = size$res,
    record = record
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  draw()

  invisible(NULL)
}


#' Export a named collection of plots to files
#'
#' @description
#' Generic replacement for the near-identical `export_*_plots` wrappers that
#' used to live one per method: builds a standardized filename for each plot
#' via `build_export_filename()` and writes it with either `save_plot()`
#' (ggplot2 objects) or `export_base_plot()` (zero-argument base-graphics draw
#' functions).
#'
#' `NULL` entries in `plots` are skipped (e.g. an optional close-up plot that
#' was not built). Entries are named for filename purposes either by
#' `names(plots)`, or by `names` when supplied; a scalar `names` is recycled
#' across every plot. When every plot shares the same name and there is more
#' than one plot, a `1`-based index is appended to disambiguate filenames
#' (matching the historical box-plot naming scheme); set `use_indexing`
#' explicitly to override this inference.
#'
#' @param plots *\[list\]* List of plots (ggplot objects, or zero-argument draw
#'   functions when `renderer = "base"`). May contain `NULL` entries, which
#'   are skipped.
#' @param base_name *\[character\]* Base filename prefix (e.g. "box_plot")
#' @param export_path *\[character\]* Directory to export to
#' @param graph_scale *\[numeric\]* Scale factor for dimensions
#' @param names *\[character, optional\]* Name to use per plot for the
#'   filename. Defaults to `names(plots)`. A scalar is recycled.
#' @param width *\[numeric\]* Unscaled plot width. Defaults to 800.
#' @param height *\[numeric\]* Unscaled plot height. Defaults to 600.
#' @param use_indexing *\[logical, optional\]* Force (or suppress) the
#'   numeric-suffix disambiguation. Defaults to auto-detection (see above).
#' @param renderer *\[character\]* Either `"ggplot"` (default, uses
#'   `save_plot()`) or `"base"` (uses `export_base_plot()` for base-graphics
#'   draw functions).
#' @param record *\[logical\]* Register the files via `record_output_file()`.
#'   Set to FALSE for throwaway preview renders (tempfiles). Defaults to TRUE.
#'
#' @return *\[character\]* One exported file path per entry in `plots`, with
#'   `NA` for skipped `NULL` entries (invisibly). Callers previewing plots
#'   pass these paths to `preview_plot()`.
export_named_plots <- function(plots,
                               base_name,
                               export_path,
                               graph_scale,
                               names = NULL,
                               width = 800,
                               height = 600,
                               use_indexing = NULL,
                               renderer = c("ggplot", "base"),
                               record = TRUE) {
  box::use(artma / libs / core / validation[validate])

  renderer <- match.arg(renderer)

  validate(
    is.list(plots),
    is.character(base_name),
    is.character(export_path),
    is.numeric(graph_scale)
  )

  ensure_export_dir(export_path)

  plot_names <- names %||% base::names(plots)
  validate(!is.null(plot_names), length(plot_names) %in% c(1, length(plots)))
  if (length(plot_names) == 1 && length(plots) > 1) {
    plot_names <- rep(plot_names, length(plots))
  }

  if (is.null(use_indexing)) {
    use_indexing <- length(plots) > 1 && length(unique(plot_names)) == 1
  }

  exported_paths <- rep(NA_character_, length(plots))

  for (i in seq_along(plots)) {
    plot <- plots[[i]]
    if (is.null(plot)) {
      next
    }

    index <- if (use_indexing) i else NULL
    filename <- build_export_filename(base_name, plot_names[i], index = index)
    full_path <- file.path(export_path, filename)

    if (renderer == "base") {
      export_base_plot(
        plot, full_path,
        width = width, height = height, graph_scale = graph_scale, record = record
      )
    } else {
      save_plot(
        plot = plot,
        path = full_path,
        width = width,
        height = height,
        scale = graph_scale,
        record = record
      )
    }

    exported_paths[i] <- full_path
  }

  invisible(exported_paths)
}


box::export(
  BASE_PLOT_DPI,
  base_plot_device_size,
  ensure_export_dir,
  build_export_filename,
  open_png_device,
  preview_needs_file,
  preview_plot,
  save_plot,
  save_plot_html,
  export_base_plot,
  export_named_plots,
  use_file_preview
)
