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
#' @param res *\[numeric\]* Resolution in pixels per inch. Defaults to 90.
#'
#' @return NULL (invisibly). Called for its side effect of opening a graphics device.
#' @keywords internal
open_png_device <- function(path, width, height, units = "px", res = 90) {
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
    is.numeric(res), res > 0
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
  record_output_file(path)

  invisible(NULL)
}


#' Save a ggplot2 plot to file
#'
#' @description
#' Wrapper around ggplot2::ggsave with artma defaults.
#'
#' @param plot *\[ggplot\]* The plot to save
#' @param path *\[character\]* Full file path (including filename)
#' @param width *\[numeric\]* Plot width
#' @param height *\[numeric\]* Plot height
#' @param scale *\[numeric\]* Scale multiplier for dimensions. Defaults to 1.
#' @param units *\[character\]* Units for width/height. Defaults to "px".
#' @param dpi *\[numeric\]* Resolution. Defaults to 150.
#'
#' @return *\[character\]* The path where the plot was saved (invisibly)
save_plot <- function(plot, path, width = 800, height = 1100, scale = 1, units = "px", dpi = 150) {
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
    is.numeric(scale), scale > 0
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
    dpi = dpi,
    device = device
  )

  # Graphics are a side effect of the method run, not part of its return value,
  # so record them: a cache hit that replays the value must not leave the
  # results directory without its plots.
  record_output_file(path)

  if (get_verbosity() >= 4) {
    cli::cli_alert_success("Exported plot to {.file {path}}")
  }

  if (isTRUE(getOption("artma.visualization.export_html", FALSE))) {
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


box::export(
  ensure_export_dir,
  build_export_filename,
  open_png_device,
  save_plot,
  save_plot_html
)
