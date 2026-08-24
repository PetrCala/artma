#' @title Visualization Color Palettes
#' @description
#' Centralized color palette definitions for all artma visualization methods.
#' Provides consistent theming across box plots, funnel plots, histograms, etc.
#'
#' Colors are derived from a small set of per-theme tokens rather than being
#' picked per method. A theme contributes an accent family (three lightness
#' steps of one hue) plus two reference hues; every method palette is assembled
#' from those, so a change to a token propagates everywhere and no two methods
#' can drift apart. `get_colors()` keeps its original per-method interface, so
#' call sites are unaffected by the derivation.

#' Valid theme names
VALID_THEMES <- c("blue", "yellow", "green", "red", "purple")

#' Per-theme color tokens
#'
#' @description
#' The source of truth for every themed color in the package.
#'
#' * `accent` carries the data itself (points, bars, fills).
#' * `accent_dark` is the same hue darkened, for outlines, medians and curves
#'   that must stay legible on top of `accent`.
#' * `accent_light` is a tint for large filled areas.
#' * `contrast` marks the primary reference line (the mean, the null).
#' * `highlight` marks a secondary reference that must not be confused with
#'   `contrast`.
#'
#' Hues are chosen so that `accent`, `contrast` and `highlight` stay separable
#' under the common forms of color vision deficiency: no theme pairs a green
#' against a red, and the three differ in lightness as well as in hue.
THEME_TOKENS <- list(
  blue = list(
    accent = "#3B6EA5",
    accent_dark = "#1F4470",
    accent_light = "#DCE6F2",
    contrast = "#C4462B",
    highlight = "#A8730A"
  ),
  yellow = list(
    accent = "#C08A1E",
    accent_dark = "#7F5A08",
    accent_light = "#F4E8CC",
    contrast = "#2F6F8F",
    highlight = "#8C4799"
  ),
  green = list(
    accent = "#3F8A5B",
    accent_dark = "#235B39",
    accent_light = "#DAEADF",
    contrast = "#8C4799",
    highlight = "#A8730A"
  ),
  red = list(
    accent = "#C0553C",
    accent_dark = "#8A3421",
    accent_light = "#F3DED8",
    contrast = "#2F6F8F",
    highlight = "#8C4799"
  ),
  purple = list(
    accent = "#7B5AA6",
    accent_dark = "#513A72",
    accent_light = "#E6DDF1",
    contrast = "#C4462B",
    highlight = "#2F6F8F"
  )
)

#' Theme-independent neutrals
#'
#' @description
#' Structural colors shared by every theme. Keeping them out of `THEME_TOKENS`
#' is deliberate: gridlines and axis text should look identical whichever accent
#' the user picked, so that switching themes recolors the data and nothing else.
NEUTRALS <- list(
  ink = "#1A1A1A",
  ink_soft = "#4A4A4F",
  muted = "#8A8A90",
  grid = "#E5E5E9",
  axis = "#B4B4BA",
  surface_alt = "#F2F2F5"
)

#' Plot surface colors for each theme
#'
#' @description
#' A near-white surface carrying roughly a two percent tint of the theme hue.
#' The panel and the plot area share this single value, so the figure reads as
#' one surface: a saturated plot background around a white panel produces a
#' colored frame, which is the strongest "spreadsheet chart" cue there is.
BACKGROUNDS <- list(
  blue = "#FAFBFD",
  yellow = "#FDFCF7",
  green = "#FAFCFA",
  red = "#FDFAF9",
  purple = "#FBFAFD"
)

#' Qualitative palette names for grouped plots
#'
#' @description
#' Brewer palette names used where a plot colors an unordered categorical
#' variable. These must be qualitative: a sequential ramp (Reds, Purples,
#' YlOrRd) implies an ordering the groups do not have, and renders its lightest
#' classes almost invisibly against a white panel.
QUALITATIVE_PALETTE <- "Dark2"

#' Map a function over every theme's tokens
#'
#' @description
#' Produces a theme-keyed list, which is the innermost level `get_colors()`
#' indexes into.
#'
#' @param f *\[function\]* Called with one entry of `THEME_TOKENS`
#' @return *\[list\]* Named by theme
#' @keywords internal
themed <- function(f) lapply(THEME_TOKENS, f)

#' Color palettes organized by visualization method
#'
#' @description
#' Derived from `THEME_TOKENS`, in the method-first shape `get_colors()`
#' expects: `PALETTES[[method]][[theme]]`, or
#' `PALETTES[[method]][[submethod]][[theme]]` for methods with variants.
PALETTES <- list(
  box_plot = themed(function(tokens) {
    list(
      outlier = tokens$accent,
      fill = tokens$accent_light,
      border = tokens$accent_dark
    )
  }),
  funnel_plot = themed(function(tokens) tokens$accent),
  t_stat_histogram = list(
    main = themed(function(tokens) tokens$accent),
    density = themed(function(tokens) tokens$accent_dark),
    critical = themed(function(tokens) tokens$contrast),
    mean = themed(function(tokens) tokens$highlight)
  ),
  # Diverging ramp for the BMA image plot: negative coefficients take the
  # contrast hue, positive ones the accent, zero the near-white midpoint.
  bma = themed(function(tokens) c(tokens$contrast, "#F7F7F7", tokens$accent_dark)),
  # STEM draws studies, the confidence interval and the stem estimate on one
  # panel, so all three need to be legible against white at once.
  stem = themed(function(tokens) c(tokens$accent, tokens$contrast, tokens$highlight)),
  prima_facie = list(
    histogram = themed(function(tokens) QUALITATIVE_PALETTE),
    density = themed(function(tokens) QUALITATIVE_PALETTE)
  ),
  bpe = list(
    miracle = themed(function(tokens) QUALITATIVE_PALETTE),
    density = themed(function(tokens) QUALITATIVE_PALETTE)
  )
)

#' Contrasting line colors (for mean/reference lines)
VLINE_COLORS <- lapply(THEME_TOKENS, function(tokens) tokens$contrast)


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
#' # Returns: list(outlier = "#3B6EA5", fill = "#DCE6F2", border = "#1F4470")
#'
#' get_colors("blue", "t_stat_histogram", "density")
#' # Returns: "#1F4470"
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


#' Get the token set for a theme
#'
#' @description
#' Direct access to the accent family and reference hues, for plots that need a
#' color the per-method palettes do not name.
#'
#' @param theme_name *\[character\]* Theme name
#' @return *\[list\]* The theme's tokens: accent, accent_dark, accent_light, contrast, highlight
get_tokens <- function(theme_name) {
  validate_theme(theme_name)
  THEME_TOKENS[[theme_name]]
}


#' Get a structural neutral color
#'
#' @description
#' Neutrals are shared by every theme. Passing no name returns the whole set.
#'
#' @param name *\[character, optional\]* One of: ink, ink_soft, muted, grid, axis, surface_alt
#' @return *\[character or list\]* The requested neutral, or all of them
get_neutral <- function(name = NULL) {
  box::use(artma / libs / core / validation[assert])

  if (is.null(name)) {
    return(NEUTRALS)
  }

  assert(is.character(name) && length(name) == 1, "name must be a single character string")
  if (!name %in% names(NEUTRALS)) {
    cli::cli_abort("Unknown neutral {.val {name}}. Must be one of: {.val {names(NEUTRALS)}}")
  }

  NEUTRALS[[name]]
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
  THEME_TOKENS,
  NEUTRALS,
  BACKGROUNDS,
  PALETTES,
  validate_theme,
  get_colors,
  get_tokens,
  get_neutral,
  get_background,
  get_vline_color
)
