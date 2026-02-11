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
    artma / libs / core / utils[get_verbosity]
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

  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width * scale,
    height = height * scale,
    units = units,
    dpi = dpi
  )

  if (get_verbosity() >= 4) {
    cli::cli_alert_success("Exported plot to {.file {path}}")
  }

  invisible(path)
}


box::export(
  ensure_export_dir,
  build_export_filename,
  save_plot
)
