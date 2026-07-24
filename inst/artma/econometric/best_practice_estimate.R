#' @title Best-practice estimate numeric core
#' @description
#' Point-estimate, linear-combination standard error, economic significance,
#' factor summary, and override-resolution helpers for the best-practice
#' estimate method. The `methods/best_practice_estimate.R` wrapper owns option
#' resolution, prompting, printing, and result assembly; the plot builders live
#' in `visualization/best_practice_estimate.R`.
NULL

box::use(
  artma / econometric / vcov[robust_vcov],
  artma / libs / core / grouping[resolve_variable_groups]
)

resolve_bpe_context <- function(df, bma_data) {
  if (!"study_id" %in% colnames(df)) {
    synthetic <- seq_len(nrow(bma_data))
    return(list(study_id = synthetic, study_label = as.character(synthetic)))
  }

  aligned_df <- NULL
  shared_cols <- intersect(colnames(bma_data), colnames(df))
  if (length(shared_cols) == ncol(bma_data)) {
    complete_rows <- stats::complete.cases(df[, shared_cols, drop = FALSE])
    if (sum(complete_rows) == nrow(bma_data)) {
      aligned_df <- df[complete_rows, , drop = FALSE]
    }
  }

  if (is.null(aligned_df) && nrow(df) == nrow(bma_data)) {
    aligned_df <- df
  }

  if (is.null(aligned_df)) {
    fallback <- seq_len(nrow(bma_data))
    return(list(study_id = fallback, study_label = as.character(fallback)))
  }

  labels <- if ("study_label" %in% colnames(aligned_df)) {
    as.character(aligned_df$study_label)
  } else {
    as.character(aligned_df$study_id)
  }

  list(
    study_id = aligned_df$study_id,
    study_label = labels
  )
}

resolve_bpe_vcov <- function(ols_model, cluster_ids = NULL) {
  robust_vcov(
    model = ols_model,
    cluster = cluster_ids,
    engine = "sandwich",
    clustered_type = "HC0",
    match_cluster_length = TRUE
  )
}

compute_bpe_point_estimate <- function(predictor_values, coef_post_mean, include_intercept) {
  intercept <- if (include_intercept && "(Intercept)" %in% names(coef_post_mean)) {
    as.numeric(coef_post_mean["(Intercept)"])
  } else {
    0
  }

  predictor_coefs <- coef_post_mean[names(predictor_values)]
  intercept + sum(predictor_coefs * predictor_values, na.rm = TRUE)
}

build_bpe_row <- function(scope, study_id, study_label, predictor_values, coef_post_mean,
                          include_intercept, vcov_matrix, z_value) {
  estimate <- compute_bpe_point_estimate(
    predictor_values = predictor_values,
    coef_post_mean = coef_post_mean,
    include_intercept = include_intercept
  )
  standard_error <- compute_linear_combo_se(
    predictor_values = predictor_values,
    include_intercept = include_intercept,
    vcov_matrix = vcov_matrix
  )

  ci_lower <- if (is.finite(estimate) && is.finite(standard_error)) {
    estimate - z_value * standard_error
  } else {
    NA_real_
  }
  ci_upper <- if (is.finite(estimate) && is.finite(standard_error)) {
    estimate + z_value * standard_error
  } else {
    NA_real_
  }

  data.frame(
    scope = scope,
    study_id = study_id,
    study_label = study_label,
    estimate = estimate,
    standard_error = standard_error,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    stringsAsFactors = FALSE
  )
}

compute_linear_combo_se <- function(predictor_values, include_intercept, vcov_matrix) {
  coef_names <- rownames(vcov_matrix)
  if (is.null(coef_names) || !length(coef_names)) {
    return(NA_real_)
  }

  c_vector <- stats::setNames(rep(0, length(coef_names)), coef_names)
  if (include_intercept && "(Intercept)" %in% coef_names) {
    c_vector["(Intercept)"] <- 1
  }

  for (var_name in names(predictor_values)) {
    if (var_name %in% coef_names && is.finite(predictor_values[[var_name]])) {
      c_vector[[var_name]] <- predictor_values[[var_name]]
    }
  }

  variance <- as.numeric(
    t(c_vector) %*% vcov_matrix[coef_names, coef_names, drop = FALSE] %*% c_vector
  )

  if (!is.finite(variance) || variance < 0) {
    return(NA_real_)
  }

  sqrt(variance)
}

#' @title Economic Significance of BMA Variables
#' @description
#' For each BMA variable, compute the change in the best-practice estimate
#' from a 1-SD change and from a min-to-max change in that variable, both as
#' a level and as a percentage of the reference best-practice estimate.
#' @keywords internal
compute_bpe_economic_significance <- function(predictors, bma_data, coef_post_mean,
                                              bpe_reference_estimate, pip_values, config,
                                              round_to, pip_threshold) {
  empty <- data.frame(
    variable = character(0),
    var_label = character(0),
    pip = numeric(0),
    sd_change = numeric(0),
    sd_change_pct = numeric(0),
    range_change = numeric(0),
    range_change_pct = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!length(predictors)) {
    return(empty)
  }

  has_reference <- is.finite(bpe_reference_estimate) && bpe_reference_estimate != 0

  rows <- lapply(predictors, function(var_name) {
    values <- as.numeric(bma_data[[var_name]])
    values <- values[is.finite(values)]
    beta <- coef_post_mean[[var_name]]

    sd_change <- if (length(values) > 1) beta * stats::sd(values) else NA_real_
    range_change <- if (length(values)) beta * (max(values) - min(values)) else NA_real_

    config_key <- find_config_key_for_var(var_name, config)
    var_label <- var_name
    if (!is.null(config_key)) {
      var_label <- config[[config_key]]$var_name_verbose %||% var_name
    }

    data.frame(
      variable = var_name,
      var_label = var_label,
      pip = as.numeric(pip_values[[var_name]]),
      sd_change = sd_change,
      sd_change_pct = if (has_reference) sd_change / bpe_reference_estimate * 100 else NA_real_,
      range_change = range_change,
      range_change_pct = if (has_reference) range_change / bpe_reference_estimate * 100 else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)

  if (!is.na(pip_threshold)) {
    out <- out[!is.na(out$pip) & out$pip >= pip_threshold, , drop = FALSE]
  }

  for (col in c("pip", "sd_change", "sd_change_pct", "range_change", "range_change_pct")) {
    out[[col]] <- round_if_finite(out[[col]], round_to)
  }

  rownames(out) <- NULL
  out
}

#' @title Best-Practice Estimate Grouped by Factor Levels
#' @description
#' For every predictor flagged via `bpe_sum_stats`/`bpe_equal`/`bpe_gltl` in
#' the data config, splits the BMA observations into factor-level groups
#' (mirroring `effect_summary_stats`) and computes the best-practice estimate
#' for each group, holding every other predictor at its configured/resolved
#' override.
#' @keywords internal
compute_bpe_factor_summary <- function(predictors, config, bma_data, coef_post_mean, include_intercept,
                                       vcov_matrix, z_value, overrides, round_to) {
  empty <- data.frame(
    scope = character(0),
    study_id = character(0),
    study_label = character(0),
    estimate = numeric(0),
    standard_error = numeric(0),
    ci_lower = numeric(0),
    ci_upper = numeric(0),
    n_obs = integer(0),
    stringsAsFactors = FALSE
  )

  rows <- list()

  for (var_name in predictors) {
    config_key <- find_config_key_for_var(var_name, config)
    if (is.null(config_key)) {
      next
    }

    var_cfg <- config[[config_key]]
    if (!is_bpe_factor_var(var_cfg)) {
      next
    }

    var_label <- var_cfg$var_name_verbose %||% var_name
    groups <- resolve_variable_groups(
      var_label = var_label,
      equal_val = var_cfg$bpe_equal,
      gltl_val = var_cfg$bpe_gltl,
      var_values = bma_data[[var_name]],
      round_to = round_to,
      auto_levels = TRUE
    )

    for (group in groups) {
      if (!length(group$row_idx)) {
        next
      }

      group_values <- compute_context_values(
        bma_data = bma_data,
        row_idx = group$row_idx,
        predictors = predictors,
        overrides = overrides
      )

      row <- build_bpe_row(
        scope = "factor",
        study_id = NA_character_,
        study_label = group$label,
        predictor_values = group_values,
        coef_post_mean = coef_post_mean,
        include_intercept = include_intercept,
        vcov_matrix = vcov_matrix,
        z_value = z_value
      )
      row$n_obs <- length(group$row_idx)
      rows[[length(rows) + 1]] <- row
    }
  }

  if (!length(rows)) {
    return(empty)
  }

  out <- do.call(rbind, rows)
  out$estimate <- round_if_finite(out$estimate, round_to)
  out$standard_error <- round_if_finite(out$standard_error, round_to)
  out$ci_lower <- round_if_finite(out$ci_lower, round_to)
  out$ci_upper <- round_if_finite(out$ci_upper, round_to)
  rownames(out) <- NULL
  out
}

compute_context_values <- function(bma_data, row_idx, predictors, overrides) {
  values <- vapply(predictors, function(var_name) {
    resolve_bpe_value(
      values = bma_data[row_idx, var_name, drop = TRUE],
      override = overrides[[var_name]]
    )
  }, numeric(1))
  values
}

resolve_bpe_value <- function(values, override) {
  numeric_values <- as.numeric(values)
  numeric_values <- numeric_values[is.finite(numeric_values)]

  if (!length(numeric_values)) {
    return(NA_real_)
  }

  if (is_bpe_override_na(override)) {
    return(mean(numeric_values, na.rm = TRUE))
  }

  parsed <- parse_bpe_override(override, allow_na = TRUE)
  if (is_bpe_override_na(parsed)) {
    return(mean(numeric_values, na.rm = TRUE))
  }

  if (is.numeric(parsed) && length(parsed) == 1 && !is.na(parsed)) {
    return(as.numeric(parsed))
  }

  switch(parsed,
    mean = mean(numeric_values, na.rm = TRUE),
    median = stats::median(numeric_values, na.rm = TRUE),
    min = min(numeric_values, na.rm = TRUE),
    max = max(numeric_values, na.rm = TRUE),
    mean(numeric_values, na.rm = TRUE)
  )
}

#' @title Detect which BPE predictors were z-scaled for BMA
#' @description
#' `get_bma_data()` standardizes every non-binary predictor before handing the
#' data to BMA, so the values `compute_context_values()` reads back out of
#' `bma_data` are on the standardized scale for those columns. This flags
#' which predictors that applies to, so the formula string can label them
#' instead of silently printing standardized values as if they were raw ones.
#' @param bma_data *\[data.frame\]* The (possibly standardized) BMA data.
#' @param predictors *\[character\]* Predictor names to check.
#' @return *\[logical\]* Named vector, TRUE where the predictor is standardized.
detect_bpe_standardized_predictors <- function(bma_data, predictors) {
  stats::setNames(
    vapply(predictors, function(var_name) length(unique(bma_data[[var_name]])) != 2, logical(1)),
    predictors
  )
}

build_bpe_formula_string <- function(coef_post_mean, predictor_values, include_intercept, round_to,
                                     standardized_predictors = NULL) {
  parts <- character(0)

  if (include_intercept && "(Intercept)" %in% names(coef_post_mean)) {
    parts <- c(parts, format(round(coef_post_mean["(Intercept)"], round_to), nsmall = round_to))
  }

  for (var_name in names(predictor_values)) {
    beta <- coef_post_mean[[var_name]]
    value <- predictor_values[[var_name]]
    if (!is.finite(beta) || !is.finite(value)) {
      next
    }
    is_standardized <- isTRUE(standardized_predictors[[var_name]])
    parts <- c(
      parts,
      sprintf(
        "%s * %s (%s%s)",
        format(round(beta, round_to), nsmall = round_to),
        format(round(value, round_to), nsmall = round_to),
        var_name,
        if (is_standardized) ", standardized" else ""
      )
    )
  }

  if (!length(parts)) {
    return("0")
  }

  paste(parts, collapse = " + ")
}

get_existing_bpe_overrides <- function(predictor_names, config) {
  box::use(artma / libs / core / utils[get_verbosity])

  overrides <- empty_bpe_override_map(predictor_names)

  for (var_name in predictor_names) {
    config_key <- find_config_key_for_var(var_name, config)
    if (is.null(config_key) || !is.list(config[[config_key]])) {
      next
    }

    raw_value <- config[[config_key]]$bpe
    parsed <- tryCatch(
      parse_bpe_override(raw_value, allow_na = TRUE, var_name = var_name),
      error = function(e) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning(
            "Ignoring invalid configured BPE override for {.field {var_name}}: {e$message}"
          )
        }
        NA
      }
    )
    overrides[[var_name]] <- parsed
  }

  overrides
}

get_bpe_recommendations <- function(predictor_names, config) {
  recommendations <- empty_bpe_override_map(predictor_names)

  for (var_name in predictor_names) {
    config_key <- find_config_key_for_var(var_name, config)
    config_entry <- if (!is.null(config_key)) config[[config_key]]
    recommendations[[var_name]] <- infer_bpe_recommendation(var_name, config_entry)
  }

  recommendations
}

infer_bpe_recommendation <- function(var_name, config_entry = NULL) {
  label <- normalize_bpe_label(
    paste(
      var_name,
      config_entry$var_name_verbose %||% "",
      config_entry$var_name_description %||% ""
    )
  )

  if (identical(var_name, "se") || grepl("\\bse\\b|standard error", label, perl = TRUE)) {
    return(0)
  }

  if (matches_any(label, c("first lag instrument", "lag instrument first", "first lag iv"))) {
    return(0)
  }
  if (matches_any(label, c("nondurable consumption", "non durable consumption"))) {
    return(1)
  }
  if (matches_any(label, c("food consumption", "food only"))) {
    return(0)
  }
  if (matches_any(label, c("time dumm", "time fixed effect"))) {
    return(1)
  }
  if (matches_any(label, c("nonseparab", "non separab"))) {
    return(1)
  }
  if (matches_any(label, c("return on capital", "capital return"))) {
    return(1)
  }
  if (matches_any(label, c("micro data", "micro study", "micro level"))) {
    return(1)
  }
  if (matches_any(label, c("log linear approximation", "log linear euler"))) {
    return(0)
  }
  if (matches_any(label, c("hall 1988", "hall normalization"))) {
    return(1)
  }
  if (matches_any(label, c("eis", "relative risk aversion"))) {
    return(1)
  }
  if (matches_any(label, c("\\bgmm\\b", "generalized method of moments"))) {
    return(1)
  }
  if (matches_any(label, c("top journal", "top tier journal"))) {
    return(1)
  }
  if (matches_any(label, c("cross sectional", "cross section unit"))) {
    return("max")
  }
  if (matches_any(label, c("years of data", "data period"))) {
    return("max")
  }
  if (matches_any(label, c("average year", "mean year"))) {
    return("max")
  }
  if (matches_any(label, c("citation", "impact factor"))) {
    return("max")
  }

  NA
}

#' @title Whether a Config Entry is Flagged for BPE Factor Grouping
#' @keywords internal
is_bpe_factor_var <- function(var_cfg) {
  if (!is.list(var_cfg)) {
    return(FALSE)
  }

  flag <- var_cfg$bpe_sum_stats
  equal <- var_cfg$bpe_equal
  gltl <- var_cfg$bpe_gltl

  any(c(isTRUE(flag), !is_bpe_override_na(equal), !is_bpe_override_na(gltl)))
}

parse_bpe_override <- function(value, allow_na = TRUE, var_name = NULL) {
  allowed_strings <- c("mean", "median", "min", "max")

  fail <- function(msg) {
    if (!is.null(var_name)) {
      cli::cli_abort("Invalid BPE override for {.field {var_name}}: {msg}")
    }
    cli::cli_abort("Invalid BPE override: {msg}")
  }

  if (is.null(value) || length(value) == 0) {
    if (allow_na) {
      return(NA)
    }
    fail("value cannot be empty")
  }

  if (length(value) == 1 && is.na(value)) {
    if (allow_na) {
      return(NA)
    }
    fail("value cannot be NA")
  }

  if (is.logical(value) && length(value) == 1) {
    return(as.numeric(value))
  }

  if (is.numeric(value) && length(value) == 1) {
    return(as.numeric(value))
  }

  if (!is.character(value) || length(value) != 1) {
    fail("value must be a scalar numeric or character")
  }

  cleaned <- tolower(trimws(value))
  if (!nzchar(cleaned) || cleaned %in% c("na", "none", "default", "null")) {
    if (allow_na) {
      return(NA)
    }
    fail("empty/default-like value is not allowed here")
  }

  if (cleaned %in% allowed_strings) {
    return(cleaned)
  }

  numeric_value <- suppressWarnings(as.numeric(cleaned))
  if (!is.na(numeric_value)) {
    return(numeric_value)
  }

  fail("use numeric, mean, median, min, max, or default")
}

format_bpe_override <- function(value) {
  if (is_bpe_override_na(value)) {
    return("default(mean)")
  }
  if (is.numeric(value)) {
    return(as.character(signif(value, 6)))
  }
  as.character(value)
}

#' @title Format a BPE Recommendation for Display
#' @description
#' Distinguishes "no recommendation" (nothing matched, so the mean fallback
#' is not a genuine recommendation) from an actual recommended value, which
#' `format_bpe_override()` alone cannot express since both currently share
#' the same NA representation.
#' @keywords internal
format_bpe_recommendation <- function(value, has_recommendation) {
  if (!isTRUE(has_recommendation)) {
    return("no recommendation")
  }
  format_bpe_override(value)
}

is_bpe_override_na <- function(value) {
  is.null(value) || length(value) == 0 || (length(value) == 1 && is.na(value))
}

round_if_finite <- function(x, digits) {
  out <- as.numeric(x)
  finite <- is.finite(out)
  out[finite] <- round(out[finite], digits)
  out
}

find_config_key_for_var <- function(var_name, config) {
  if (var_name %in% names(config)) {
    return(var_name)
  }

  var_key <- make.names(var_name)
  if (var_key %in% names(config)) {
    return(var_key)
  }

  matches <- names(config)[vapply(config, function(entry) {
    is.list(entry) && is.character(entry$var_name) && identical(entry$var_name, var_name)
  }, logical(1))]

  if (length(matches)) {
    return(matches[1])
  }

  NULL
}

empty_bpe_override_map <- function(var_names) {
  stats::setNames(lapply(var_names, function(x) NA), var_names)
}

normalize_bpe_label <- function(label) {
  cleaned <- gsub("[^a-zA-Z0-9]+", " ", label)
  tolower(trimws(cleaned))
}

matches_any <- function(label, patterns) {
  any(vapply(patterns, function(pattern) {
    grepl(pattern, label, perl = TRUE)
  }, logical(1)))
}

box::export(
  resolve_bpe_context,
  resolve_bpe_vcov,
  compute_bpe_point_estimate,
  compute_linear_combo_se,
  build_bpe_row,
  compute_bpe_economic_significance,
  compute_bpe_factor_summary,
  compute_context_values,
  resolve_bpe_value,
  detect_bpe_standardized_predictors,
  build_bpe_formula_string,
  get_existing_bpe_overrides,
  get_bpe_recommendations,
  infer_bpe_recommendation,
  is_bpe_factor_var,
  parse_bpe_override,
  format_bpe_override,
  format_bpe_recommendation,
  is_bpe_override_na,
  round_if_finite,
  find_config_key_for_var
)
