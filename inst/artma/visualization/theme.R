#' @title Visualization Theme Utilities
#' @description
#' Shared ggplot2 theme construction for artma visualizations.

#' Get ggplot2 theme for visualizations
#'
#' @description
#' Constructs a consistent ggplot2 theme based on the selected color theme.
#' Used across all artma visualization methods for visual consistency.
#'
#' The result is a *complete* theme, built on `ggplot2::theme_minimal()`. An
#' earlier version returned a bare `theme()` that callers added on top of the
#' default `theme_grey()`, which left grey panel fills, grey facet strips and
#' grey legend keys showing through wherever an element was not named
#' explicitly.
#'
#' Panel and plot share one near-white surface. Painting the plot background in
#' a saturated tint while the panel stays white draws a colored frame around
#' every figure, which is the strongest visual cue that separates a spreadsheet
#' chart from a publication one. The theme hue survives in the data itself, via
#' `artma/visualization/colors`.
#'
#' Per-tick x-axis coloring (e.g. highlighting the mean or critical-value
#' ticks) is done by passing `tick_colors`: ggplot2 recycles a vectorized
#' `colour` across the axis labels, so no rich-text renderer is involved and
#' tick labels stay plain text.
#'
#' @param theme_name *\[character\]* One of: blue, yellow, green, red, purple
#' @param tick_colors *\[character, optional\]* One color per x-axis tick, in
#'   break order. Must line up with the breaks passed to the scale. Defaults to
#'   the standard axis-text color for every tick.
#' @param base_size *\[numeric\]* Base font size in points. Every other size is
#'   expressed relative to it, so a single value rescales the whole figure.
#'   Defaults to 10, which suits the nominal export canvas of roughly five
#'   inches; raise it for a larger figure rather than editing single elements.
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
get_theme <- function(theme_name, tick_colors = NULL, base_size = 10) {
  box::use(
    artma / libs / core / validation[validate],
    artma / visualization / colors[validate_theme, get_background, get_neutral]
  )

  validate_theme(theme_name)
  validate(is.numeric(base_size), length(base_size) == 1, base_size > 0)

  surface <- get_background(theme_name)
  neutral <- get_neutral()

  if (is.null(tick_colors) || length(tick_colors) == 0) {
    tick_colors <- neutral$ink_soft
  }
  validate(is.character(tick_colors), !anyNA(tick_colors))

  rel <- ggplot2::rel
  margin <- ggplot2::margin
  element_text <- ggplot2::element_text
  element_line <- ggplot2::element_line
  element_rect <- ggplot2::element_rect
  element_blank <- ggplot2::element_blank

  half <- base_size / 2

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Text ---------------------------------------------------------------
      axis.text.x = element_text(
        colour = tick_colors, size = rel(0.9),
        margin = margin(t = 0.4 * half)
      ),
      axis.text.y = element_text(
        colour = neutral$ink_soft, size = rel(0.9), hjust = 1,
        margin = margin(r = 0.4 * half)
      ),
      axis.title.x = element_text(
        colour = neutral$ink, size = rel(1), margin = margin(t = half)
      ),
      axis.title.y = element_text(
        colour = neutral$ink, size = rel(1), angle = 90, margin = margin(r = half)
      ),

      # Axes ---------------------------------------------------------------
      axis.line = element_blank(),
      axis.ticks = element_line(colour = neutral$axis, linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(half / 3, "pt"),

      # Panel --------------------------------------------------------------
      panel.background = element_rect(fill = surface, colour = NA),
      panel.border = element_blank(),
      # Both directions, unlike the previous x-only grid: a funnel plot is read
      # across precision just as much as across the effect.
      panel.grid.major = element_line(colour = neutral$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # Plot ---------------------------------------------------------------
      plot.background = element_rect(fill = surface, colour = NA),
      plot.title = element_text(
        colour = neutral$ink, size = rel(1.15), face = "bold", hjust = 0,
        margin = margin(b = 0.5 * half)
      ),
      plot.subtitle = element_text(
        colour = neutral$ink_soft, size = rel(0.9), hjust = 0,
        margin = margin(b = half)
      ),
      plot.caption = element_text(
        colour = neutral$muted, size = rel(0.8), hjust = 1,
        margin = margin(t = half)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(half, half, half, half),

      # Legend -------------------------------------------------------------
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(
        colour = neutral$ink_soft, size = rel(0.85), hjust = 0
      ),
      legend.text = element_text(colour = neutral$ink_soft, size = rel(0.85)),

      # Facets -------------------------------------------------------------
      strip.background = element_rect(fill = neutral$surface_alt, colour = NA),
      strip.text = element_text(
        colour = neutral$ink, size = rel(0.85), face = "bold",
        margin = margin(half / 2, half / 2, half / 2, half / 2)
      )
    )
}


box::export(get_theme)
