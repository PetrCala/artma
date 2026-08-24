#' @title Axis tick formatting helpers
#' @description Shared tick formatting and thinning used by plot methods.
NULL

#' Upper bound on how many labels an x axis can carry before they collide.
#'
#' Tick labels are rendered at 12pt on plots exported around 800px wide, which
#' leaves room for roughly this many labels. Tick generators use it to derive a
#' minimum separation from the data range, so wide-ranging data does not end up
#' with labels printed on top of each other.
MAX_AXIS_TICKS <- 14L

#' Tolerated rounding error in a tick label, as a fraction of the larger of
#' the tick's own magnitude and the spacing between ticks.
#'
#' Loose enough that a data maximum of 368.46 is labelled `368`, tight enough
#' that a critical value of 1.96 is never labelled `2`.
TICK_LABEL_TOLERANCE <- 0.01

#' Choose how many decimals a set of tick labels needs
#'
#' @description
#' Picks the fewest decimals that label the ticks honestly: every label stays
#' distinct, no non-zero tick prints as `0`, and no label misstates its tick by
#' more than `TICK_LABEL_TOLERANCE` of the larger of that tick's magnitude and
#' the spacing between ticks. Widely spaced ticks in the hundreds get integer
#' labels; a t-statistic critical value keeps its `1.96`; effects measured in
#' thousandths keep the digits that distinguish them.
#'
#' @param x *\[numeric\]* Tick values
#' @return *\[integer\]* Number of decimals to round labels to
#' @keywords internal
tick_label_digits <- function(x) {
  finite <- x[is.finite(x)]
  if (length(finite) == 0) {
    return(0L)
  }

  gaps <- diff(sort(finite))
  gaps <- gaps[gaps > 0]
  min_gap <- if (length(gaps) > 0) min(gaps) else 0
  tolerance <- TICK_LABEL_TOLERANCE * pmax(abs(finite), min_gap)

  for (digits in 0:6) {
    rounded <- round(finite, digits)
    if (anyDuplicated(rounded) > 0) next
    if (any(rounded == 0 & finite != 0)) next
    if (any(abs(rounded - finite) > tolerance)) next
    return(as.integer(digits))
  }

  6L
}

#' Format tick values as integers or decimals appropriately
#'
#' @description
#' Displays integer values without decimal points and floats with
#' minimal necessary precision for cleaner axis labels. When `digits` is not
#' supplied it is derived from the tick set via `tick_label_digits()`, so the
#' labels carry just enough precision to stay distinguishable.
#'
#' @param x *\[numeric\]* Tick values
#' @param digits *\[integer, optional\]* Decimals to round to. Defaults to
#'   `tick_label_digits(x)`.
#' @return *\[character\]* Formatted labels
#' @keywords internal
format_tick_labels <- function(x, digits = NULL) {
  if (is.null(digits)) {
    digits <- tick_label_digits(x)
  }

  vapply(x, function(val) {
    if (is.na(val)) {
      return(NA_character_)
    }
    format(round(val, digits), trim = TRUE, scientific = FALSE)
  }, FUN.VALUE = character(1))
}

#' Derive the minimum separation two ticks need over a data range
#'
#' @description
#' Label collisions scale with the data range, not with the tick interval: a
#' range of 400 fits about `MAX_AXIS_TICKS` labels whatever interval the grid
#' uses, because the labels get wider as the numbers do. A degenerate range
#' needs no separation; `thin_ticks()` still collapses the identical ticks it
#' produces.
#'
#' @param range_size *\[numeric\]* The range of the data being ticked
#' @return *\[numeric\]* Minimum distance to keep between ticks
#' @keywords internal
tick_min_separation <- function(range_size) {
  box::use(artma / libs / core / validation[validate])

  validate(is.numeric(range_size), length(range_size) == 1)

  if (!is.finite(range_size) || range_size <= 0) {
    return(0)
  }

  range_size / MAX_AXIS_TICKS
}

#' Drop ticks that would collide, keeping the most informative ones
#'
#' @description
#' Walks the ticks in descending priority order and keeps a tick only when it
#' stays at least `min_distance` away from every tick already kept. Priority is
#' what decides a collision: a mean or critical-value tick outranks a data
#' bound, which outranks a regular grid tick, so the meaningful markers survive
#' and the filler is what gets dropped. Ties are broken by input order.
#'
#' @param ticks *\[numeric\]* Candidate tick positions
#' @param min_distance *\[numeric\]* Minimum distance to keep between ticks.
#'   Non-finite or non-positive values keep every tick.
#' @param priority *\[numeric, optional\]* Priority per tick, higher wins.
#'   Defaults to equal priority for all ticks.
#'
#' @return *\[integer\]* Indices of the kept ticks, ordered by tick value. Use
#'   them to subset both the ticks and any parallel vector (e.g. colors).
#' @keywords internal
thin_ticks <- function(ticks, min_distance, priority = NULL) {
  box::use(artma / libs / core / validation[validate])

  validate(is.numeric(ticks), is.numeric(min_distance), length(min_distance) == 1)

  n <- length(ticks)
  if (n == 0) {
    return(integer(0))
  }

  if (is.null(priority)) priority <- rep(0, n)
  validate(is.numeric(priority), length(priority) == n)

  finite <- which(is.finite(ticks))
  if (!is.finite(min_distance) || min_distance <= 0) {
    # No separation required, but identical ticks would still draw duplicate
    # labels, so collapse them, keeping the highest-priority one.
    ordered <- finite[order(ticks[finite], -priority[finite])]
    return(ordered[!duplicated(ticks[ordered])])
  }

  kept <- integer(0)
  for (i in finite[order(-priority[finite], finite)]) {
    if (length(kept) > 0 && any(abs(ticks[kept] - ticks[i]) < min_distance)) next
    kept <- c(kept, i)
  }

  kept[order(ticks[kept])]
}

#' Resolve a tick interval for a data range
#'
#' @description
#' Picks a round interval (a 1, 2, 5 or 10 times a power of ten) that splits
#' the range into roughly `target_ticks` steps. Being scale-free is the point:
#' effects measured in thousandths and effects measured in hundreds both get a
#' sensible number of round ticks, with no table of magnitudes to fall off the
#' end of.
#'
#' @param range_size *\[numeric\]* The range of the data being ticked
#' @param target_ticks *\[numeric\]* Roughly how many intervals to aim for.
#'   Defaults to 8, which leaves headroom under `MAX_AXIS_TICKS` for the
#'   bound, mean and critical-value ticks added alongside the grid.
#'
#' @return *\[numeric\]* The resolved tick interval, always positive
#' @keywords internal
resolve_tick_interval <- function(range_size, target_ticks = 8) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.numeric(range_size), length(range_size) == 1,
    is.numeric(target_ticks), length(target_ticks) == 1, target_ticks > 0
  )

  if (!is.finite(range_size) || range_size <= 0) {
    return(1)
  }

  raw <- range_size / target_ticks
  magnitude <- 10^floor(log10(raw))
  normalized <- raw / magnitude

  step <- if (normalized <= 1.5) {
    1
  } else if (normalized <= 3) {
    2
  } else if (normalized <= 7) {
    5
  } else {
    10
  }

  step * magnitude
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
  MAX_AXIS_TICKS,
  TICK_LABEL_TOLERANCE,
  format_tick_labels,
  generate_regular_ticks,
  resolve_tick_interval,
  thin_ticks,
  tick_label_digits,
  tick_min_separation
)
