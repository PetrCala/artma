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
#' Wraps tick labels in HTML span elements with color styling. Historically
#' rendered via ggtext::element_markdown() for per-tick coloring (e.g.,
#' highlighting the mean tick in a different color); the theme now uses plain
#' ggplot2::element_text(), so the HTML markup is displayed as literal text
#' rather than rendered as color.
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

#' Resolve a tick interval from a breakpoint table
#'
#' @description
#' Shared stepwise lookup used by plot-specific tick generators: the first
#' breakpoint that the data range does not exceed determines the interval,
#' falling back to `fallback` when the range exceeds every breakpoint.
#'
#' @param range_size *\[numeric\]* The range of the data being ticked
#' @param breakpoints *\[numeric\]* Ascending upper bounds for each interval
#' @param intervals *\[numeric\]* Interval to use for each breakpoint
#'   (same length as `breakpoints`)
#' @param fallback *\[numeric\]* Interval used when `range_size` exceeds every
#'   breakpoint
#'
#' @return *\[numeric\]* The resolved tick interval
#' @keywords internal
resolve_tick_interval <- function(range_size, breakpoints, intervals, fallback) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.numeric(range_size), length(range_size) == 1,
    is.numeric(breakpoints),
    is.numeric(intervals), length(breakpoints) == length(intervals),
    is.numeric(fallback), length(fallback) == 1
  )

  for (i in seq_along(breakpoints)) {
    if (range_size <= breakpoints[i]) {
      return(intervals[i])
    }
  }

  fallback
}

#' Generate evenly spaced regular ticks within bounds
#'
#' @description
#' Shared stepping loop used by plot-specific tick generators: starting from
#' the first multiple of `interval` at or above `lower`, walks up to `upper`
#' by `interval`, keeping a candidate only when it stays `edge_distance` away
#' from both bounds and (if any `special_values` are given) `special_distance`
#' away from all of them. Exact duplicates are left for the caller to remove
#' via `sort(unique(...))`, matching prior per-plot behavior.
#'
#' @param lower *\[numeric\]* Lower bound
#' @param upper *\[numeric\]* Upper bound
#' @param interval *\[numeric\]* Step size between candidate ticks
#' @param edge_distance *\[numeric\]* Minimum distance a candidate must keep
#'   from `lower` and `upper`
#' @param special_values *\[numeric\]* Values (e.g. mean, critical values) that
#'   candidates must also keep clear of. Defaults to none.
#' @param special_distance *\[numeric\]* Minimum distance from `special_values`.
#'   Defaults to `edge_distance`.
#' @param upper_inclusive *\[logical\]* Whether `upper` itself is a valid
#'   candidate position. Defaults to `FALSE`.
#'
#' @return *\[numeric\]* The generated regular tick positions (may be empty)
#' @keywords internal
generate_regular_ticks <- function(lower,
                                   upper,
                                   interval,
                                   edge_distance,
                                   special_values = numeric(0),
                                   special_distance = edge_distance,
                                   upper_inclusive = FALSE) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.numeric(lower), length(lower) == 1,
    is.numeric(upper), length(upper) == 1,
    is.numeric(interval), length(interval) == 1, interval > 0,
    is.numeric(edge_distance), length(edge_distance) == 1,
    is.numeric(special_values),
    is.numeric(special_distance), length(special_distance) == 1,
    is.logical(upper_inclusive)
  )

  ticks <- numeric(0)
  current <- ceiling(lower / interval) * interval

  in_range <- function(x) if (upper_inclusive) x <= upper else x < upper

  while (in_range(current)) {
    far_from_edges <- abs(current - lower) >= edge_distance &&
      abs(current - upper) >= edge_distance
    far_from_special <- if (length(special_values)) {
      all(abs(current - special_values) >= special_distance)
    } else {
      TRUE
    }

    if (far_from_edges && far_from_special) {
      ticks <- c(ticks, current)
    }
    current <- current + interval
  }

  ticks
}

box::export(
  format_tick_labels,
  format_colored_tick_labels,
  resolve_tick_interval,
  generate_regular_ticks
)
