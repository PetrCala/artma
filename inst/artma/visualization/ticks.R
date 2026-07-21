#' @title Axis tick formatting helpers
#' @description Shared tick-label formatting used by plot methods.
NULL

#' Format tick values as integers or decimals appropriately
#'
#' @description
#' Displays integer values without decimal points and floats with
#' minimal necessary precision for cleaner axis labels.
#'
#' @param x *\[numeric\]* Tick values
#' @return *\[character\]* Formatted labels
#' @keywords internal
format_tick_labels <- function(x) {
  vapply(x, function(val) {
    if (is.na(val)) {
      return(NA_character_)
    }
    if (val == floor(val)) {
      as.character(as.integer(val))
    } else {
      format(round(val, 2), nsmall = 0, trim = TRUE)
    }
  }, FUN.VALUE = character(1))
}

#' Format tick labels with HTML color spans
#'
#' @description
#' Wraps tick labels in HTML span elements with color styling for use with
#' ggtext::element_markdown(). This enables per-tick coloring (e.g., highlighting
#' the mean tick in a different color).
#'
#' @param ticks *\[numeric\]* Tick values
#' @param colors *\[character\]* Color for each tick (same length as ticks)
#'
#' @return *\[character\]* HTML-formatted labels
#' @keywords internal
format_colored_tick_labels <- function(ticks, colors) {
  labels <- format_tick_labels(ticks)

  colored <- sprintf("<span style='color:%s'>%s</span>", colors, labels)
  colored[is.na(labels)] <- NA_character_
  colored
}

box::export(
  format_tick_labels,
  format_colored_tick_labels
)
