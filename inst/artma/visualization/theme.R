#' @title Visualization Theme Utilities
#' @description
#' Shared ggplot2 theme construction for artma visualizations.

#' Get ggplot2 theme for visualizations
#'
#' @description
#' Constructs a consistent ggplot2 theme based on the selected color theme.
#' Used across all artma visualization methods for visual consistency.
#'
#' Per-tick x-axis coloring (e.g. highlighting the mean or critical-value
#' ticks) is done by passing `tick_colors`: ggplot2 recycles a vectorized
#' `colour` across the axis labels, so no rich-text renderer is involved and
#' tick labels stay plain text.
#'
#' @param theme_name *\[character\]* One of: blue, yellow, green, red, purple
#' @param tick_colors *\[character, optional\]* One color per x-axis tick, in
#'   break order. Must line up with the breaks passed to the scale. Defaults to
#'   black for every tick.
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   get_theme("blue")
#' }
get_theme <- function(theme_name, tick_colors = NULL) {
  box::use(
    artma / libs / core / validation[validate],
    artma / visualization / colors[validate_theme, get_background]
  )

  validate_theme(theme_name)
  background_color <- get_background(theme_name)

  if (is.null(tick_colors) || length(tick_colors) == 0) {
    tick_colors <- "black"
  }
  validate(is.character(tick_colors), !anyNA(tick_colors))

  ggplot2::theme(
    axis.line = ggplot2::element_line(color = "black", linewidth = 0.5, linetype = "solid"),
    axis.text.x = ggplot2::element_text(color = tick_colors, size = 12),
    axis.text.y = ggplot2::element_text(color = "black", size = 12),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major.x = ggplot2::element_line(color = background_color),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = background_color)
  )
}


box::export(get_theme)
