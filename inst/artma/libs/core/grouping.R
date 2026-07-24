#' @title Shared equal/gltl variable-group resolver
#' @description
#' Splits a numeric variable's values into labelled groups driven by a data
#' config entry, using the same equal / greater-or-less-than (gltl) semantics
#' shared by `effect_summary_stats` and `best_practice_estimate`. Extracting the
#' logic here keeps a single source of truth for label formatting, threshold
#' resolution (literal, "mean", "median") and the greater-equal / less-than tie
#' break.
box::use(
  stats
)

#' @title Whether a group-config value is unset
#' @description
#' A config value counts as "not set" when it is `NULL`, empty, or a single
#' `NA`. Used for the `equal` / `gltl` keys, which are `NA` when the split is
#' not requested. Mirrors the historical `is_bpe_override_na` predicate so the
#' `best_practice_estimate` call site keeps its exact NULL/empty tolerance while
#' `effect_summary_stats`, which previously used a bare `is.na()`, gains the same
#' (strictly safer) guard without any change for its always-present `NA` values.
#' @param value *\[any\]* Config value to test.
#' @return *\[logical(1)\]* `TRUE` when the value is unset.
#' @keywords internal
is_group_value_na <- function(value) {
  is.null(value) || length(value) == 0 || (length(value) == 1 && is.na(value))
}

#' @title Format a group-boundary value for a label
#' @description
#' Numeric boundaries are rounded to `round_to` decimals; anything else is
#' coerced with `as.character()`.
#' @param value *\[numeric|character\]* Boundary value.
#' @param round_to *\[integer\]* Decimals to round numeric values to.
#' @return *\[character(1)\]* Display string.
#' @keywords internal
format_group_value <- function(value, round_to) {
  if (is.numeric(value)) {
    return(as.character(round(value, round_to)))
  }
  as.character(value)
}

#' @title Resolve a gltl threshold expression to a numeric value
#' @description
#' A literal numeric threshold is returned as-is. The strings "mean" and
#' "median" are resolved against `threshold_values` (NA values dropped first);
#' any other string is parsed with `as.numeric()`. Empty basis data yields
#' `NA_real_` for the "mean"/"median" forms.
#'
#' The `threshold_values` argument is what lets the two call sites keep their
#' historically different bases: `effect_summary_stats` computes the mean/median
#' over rows whose effect and study size are finite, while
#' `best_practice_estimate` uses every non-NA value of the column.
#' @param gltl_val *\[numeric|character\]* Threshold value or "mean"/"median".
#' @param threshold_values *\[numeric\]* Values the mean/median is taken over.
#' @return *\[numeric(1)\]* Resolved threshold, or `NA_real_` when unresolvable.
#' @keywords internal
resolve_group_threshold <- function(gltl_val, threshold_values) {
  if (!is.character(gltl_val)) {
    return(as.numeric(gltl_val))
  }

  filtered <- threshold_values[!is.na(threshold_values)]
  switch(gltl_val,
    mean = if (length(filtered)) mean(filtered) else NA_real_,
    median = if (length(filtered)) stats::median(filtered) else NA_real_,
    suppressWarnings(as.numeric(gltl_val))
  )
}

#' @title Resolve a variable's values into equal / gltl split groups
#' @description
#' Produces the labelled groups for a single config-flagged variable:
#'
#' * an exact-match group when `equal_val` is set (`var == equal_val`);
#' * greater-equal / less-than groups when `gltl_val` is set and its threshold
#'   resolves to a non-NA number: values `>= threshold` and `< threshold` (ties
#'   at the threshold fall in the greater-equal group);
#' * when neither is set and `auto_levels` is `TRUE`, one group per distinct
#'   level, but only when the number of distinct non-NA levels is in
#'   `[2, max_auto_levels]`; otherwise an empty list.
#'
#' Both call sites share this equal/gltl core. Two behaviours stay under caller
#' control via parameters so migration is exact:
#'
#' * `threshold_values` selects the basis for "mean"/"median" thresholds (see
#'   [resolve_group_threshold]).
#' * `auto_levels` enables the per-level fallback used by
#'   `best_practice_estimate`; `effect_summary_stats` leaves it off and applies
#'   its own all-data fallback outside this helper.
#'
#' One historical divergence is intentionally dropped because it is not
#' observable: `effect_summary_stats` previously suppressed the " = " separator
#' for an empty-string `equal_val`, but it only ever groups numeric columns, so
#' that branch produced an empty (never-emitted) group. This helper always
#' formats an equal label as `"<label> = <value>"`.
#' @param var_label *\[character(1)\]* Human-readable variable label.
#' @param equal_val *\[numeric|character|NA\]* Exact-match value, or unset.
#' @param gltl_val *\[numeric|character|NA\]* Threshold value or "mean"/"median",
#'   or unset.
#' @param var_values *\[numeric\]* The variable's values; `row_idx` indexes into
#'   this vector.
#' @param round_to *\[integer\]* Decimals for label formatting.
#' @param threshold_values *\[numeric, optional\]* Basis for mean/median
#'   thresholds. Defaults to `var_values`.
#' @param auto_levels *\[logical(1), optional\]* Enable the per-level fallback.
#'   Defaults to `FALSE`.
#' @param max_auto_levels *\[integer(1), optional\]* Upper bound on distinct
#'   levels for the fallback. Defaults to `12L`.
#' @return *\[list\]* A list of `list(label, row_idx)` entries, where `row_idx`
#'   indexes into `var_values`.
#' @keywords internal
resolve_variable_groups <- function(var_label, equal_val, gltl_val, var_values, round_to,
                                    threshold_values = var_values,
                                    auto_levels = FALSE, max_auto_levels = 12L) {
  groups <- list()

  if (!is_group_value_na(equal_val)) {
    groups[[length(groups) + 1]] <- list(
      label = paste0(var_label, " = ", format_group_value(equal_val, round_to)),
      row_idx = which(!is.na(var_values) & var_values == equal_val)
    )
  }

  if (!is_group_value_na(gltl_val)) {
    threshold <- resolve_group_threshold(gltl_val, threshold_values)
    if (!is.na(threshold)) {
      groups[[length(groups) + 1]] <- list(
        label = paste0(var_label, " >= ", format_group_value(threshold, round_to)),
        row_idx = which(!is.na(var_values) & var_values >= threshold)
      )
      groups[[length(groups) + 1]] <- list(
        label = paste0(var_label, " < ", format_group_value(threshold, round_to)),
        row_idx = which(!is.na(var_values) & var_values < threshold)
      )
    }
  }

  if (length(groups)) {
    return(groups)
  }

  if (!isTRUE(auto_levels)) {
    return(list())
  }

  levels_present <- sort(unique(stats::na.omit(var_values)))
  if (length(levels_present) < 2 || length(levels_present) > max_auto_levels) {
    return(list())
  }

  lapply(levels_present, function(level_value) {
    list(
      label = paste0(var_label, " = ", format_group_value(level_value, round_to)),
      row_idx = which(!is.na(var_values) & var_values == level_value)
    )
  })
}

box::export(
  is_group_value_na,
  format_group_value,
  resolve_group_threshold,
  resolve_variable_groups
)
