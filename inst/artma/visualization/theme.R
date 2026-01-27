#' @title Visualization Theme Utilities
#' @description
#' Shared ggplot2 theme construction for artma visualizations.

#' Get ggplot2 theme for visualizations
#'
#' @description
#' Constructs a consistent ggplot2 theme based on the selected color theme.
#' Used across all artma visualization methods for visual consistency.
#'
#' @param theme_name *\[character\]* One of: blue, yellow, green, red, purple
#' @param x_axis_tick_text *\[character\]* Color for x-axis tick text. Defaults to "black".
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
get_theme <- function(theme_name, x_axis_tick_text = "black") {
  box::use(artma / visualization / colors[validate_theme, get_background])

  validate_theme(theme_name)
  background_color <- get_background(theme_name)

  ggplot2::theme(
    axis.line = ggplot2::element_line(color = "black", linewidth = 0.5, linetype = "solid"),
    axis.text.x = ggplot2::element_text(color = x_axis_tick_text, size = 12),
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
