#' @title Visualization Color Palettes
#' @description
#' Centralized color palette definitions for all artma visualization methods.
#' Provides consistent theming across box plots, funnel plots, histograms, etc.

#' Valid theme names
VALID_THEMES <- c("blue", "yellow", "green", "red", "purple")

#' Background colors for each theme
BACKGROUNDS <- list(

  blue = "#DCEEF3",
  yellow = "#FFFFD1",
  green = "#D1FFD1",
  red = "#FFD1D1",
  purple = "#E6D1FF"
)

#' Color palettes organized by visualization method
PALETTES <- list(
  box_plot = list(
    blue = list(outlier = "#005CAB", fill = "#e6f3ff", border = "#0d4ed1"),
    yellow = list(outlier = "#AB9800", fill = "#FFF5CC", border = "#D1B00D"),
    green = list(outlier = "#009B0F", fill = "#CCF3D1", border = "#0DD146"),
    red = list(outlier = "#AB0000", fill = "#FFCCCC", border = "#D10D0D"),
    purple = list(outlier = "#6A0DAB", fill = "#EAD6F5", border = "#900DAB")
  ),
  funnel_plot = list(
    blue = "#1261ff",
    yellow = "#D1B00D",
    green = "#00FF00",
    red = "#FF0000",
    purple = "#800080"
  ),
  t_stat_histogram = list(
    main = list(
      blue = "#1261ff",
      yellow = "#D1B00D",
      green = "#00FF00",
      red = "#FF0000",
      purple = "#800080"
    ),
    density = list(
      blue = "#013091",
      yellow = "#b89b00",
      green = "#008000",
      red = "#b30000",
      purple = "#660066"
    ),
    critical = list(
      blue = "#D10D0D",
      yellow = "#0d4ed1",
      green = "#D10D0D",
      red = "#0d4ed1",
      purple = "#0d4ed1"
    ),
    mean = list(
      blue = "darkorange",
      yellow = "darkgreen",
      green = "darkorange",
      red = "darkgreen",
      purple = "darkgreen"
    )
  ),
  bma = list(
    blue = c("#005CAB", "white", "#844ec7"),
    yellow = c("#AB9800", "white", "#009B0F"),
    green = c("#009B0F", "white", "#AB0000"),
    red = c("#AB0000", "white", "#6A0DAB"),
    purple = c("#6A0DAB", "white", "#005CAB")
  ),
  prima_facie = list(
    histogram = list(
      blue = "Paired",
      yellow = "YlOrRd",
      green = "Set2",
      red = "Reds",
      purple = "Purples"
    ),
    density = list(
      blue = "Paired",
      yellow = "RdYlBu",
      green = "Paired",
      red = "RdBu",
      purple = "Purples"
    )
  ),
  stem = list(
    blue = c("#005CAB", "#e8e813", "#4a19bd"),
    yellow = c("#AB9800", "#009B0F", "#4dacfa"),
    green = c("#009B0F", "#AB0000", "#423091"),
    red = c("#AB0000", "#6A0DAB", "#8cb1fa"),
    purple = c("#6A0DAB", "#005CAB", "#fa8ce6")
  ),
  bpe = list(
    miracle = list(
      blue = "Paired",
      yellow = "YlOrRd",
      green = "Set2",
      red = "Reds",
      purple = "Purples"
    ),
    density = list(
      blue = "Paired",
      yellow = "RdYlBu",
      green = "Paired",
      red = "RdBu",
      purple = "Purples"
    )
  )
)

#' Contrasting line colors (for mean/reference lines)
VLINE_COLORS <- list(
  blue = "#D10D0D",
  yellow = "#0d4ed1",
  green = "#D10D0D",
  red = "#0d4ed1",
  purple = "#0d4ed1"
)


#' Validate theme name
#'
#' @param theme_name *\[character\]* Theme name to validate
#' @return TRUE if valid, otherwise throws error
#' @keywords internal
validate_theme <- function(theme_name) {
  box::use(artma / libs / core / validation[assert])

  assert(
    is.character(theme_name) && length(theme_name) == 1,
    "theme_name must be a single character string"
  )
  assert(
    theme_name %in% VALID_THEMES,
    paste0("Invalid theme: '", theme_name, "'. Must be one of: ", paste(VALID_THEMES, collapse = ", "))
  )

  TRUE
}


#' Get colors for a visualization method
#'
#' @param theme_name *\[character\]* One of: blue, yellow, green, red, purple
#' @param method *\[character\]* Visualization method (e.g., "box_plot", "funnel_plot")
#' @param submethod *\[character, optional\]* Submethod for methods with variants (e.g., "main", "density")
#'
#' @return Color specification (list, vector, or character depending on method)
#'
#' @examples
#' \dontrun{
#' get_colors("blue", "box_plot")
#' # Returns: list(outlier = "#005CAB", fill = "#e6f3ff", border = "#0d4ed1")
#'
#' get_colors("blue", "t_stat_histogram", "density")
#' # Returns: "#013091"
#' }
get_colors <- function(theme_name, method, submethod = NULL) {
  box::use(artma / libs / core / validation[assert])

  validate_theme(theme_name)
  assert(is.character(method) && length(method) == 1, "method must be a single character string")

  if (!method %in% names(PALETTES)) {
    cli::cli_abort("Unknown visualization method: {.val {method}}")
  }

  method_palette <- PALETTES[[method]]

  if (!is.null(submethod)) {
    assert(is.character(submethod) && length(submethod) == 1, "submethod must be a single character string")

    if (!submethod %in% names(method_palette)) {
      cli::cli_abort("Unknown submethod {.val {submethod}} for method {.val {method}}")
    }

    theme_palette <- method_palette[[submethod]]
    if (!theme_name %in% names(theme_palette)) {
      cli::cli_abort("Theme {.val {theme_name}} not defined for {method}/{submethod}")
    }

    return(theme_palette[[theme_name]])
  }

  if (!theme_name %in% names(method_palette)) {
    cli::cli_abort("Theme {.val {theme_name}} not defined for method {.val {method}}")
  }

  method_palette[[theme_name]]
}


#' Get background color for a theme
#'
#' @param theme_name *\[character\]* Theme name
#' @return *\[character\]* Hex color code for background
get_background <- function(theme_name) {
  validate_theme(theme_name)
  BACKGROUNDS[[theme_name]]
}


#' Get contrasting line color for a theme
#'
#' @description
#' Returns a color that contrasts well with the theme for mean lines,
#' reference lines, etc.
#'
#' @param theme_name *\[character\]* Theme name
#' @return *\[character\]* Hex color code
get_vline_color <- function(theme_name) {
  validate_theme(theme_name)
  VLINE_COLORS[[theme_name]]
}


box::export(
  VALID_THEMES,
  BACKGROUNDS,
  PALETTES,
  validate_theme,
  get_colors,
  get_background,
  get_vline_color
)
