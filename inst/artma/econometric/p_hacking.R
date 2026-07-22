#' @title P-hacking test helpers
#' @description
#' Helper functions for comprehensive p-hacking detection tests.
#' Includes Caliper tests (Gerber & Malhotra, 2008), Elliott tests (2022),
#' and MAIVE estimator (Irsova et al., 2023).
NULL

box::use(
  stats[pchisq, ecdf, coef],
  artma / libs / core / validation[validate, assert],
  artma / libs / formatting / results[
    format_number,
    format_se,
    format_ci,
    significance_mark
  ],
  artma / calc / methods / elliott[
    simulate_cdfs_parallel,
    binomial_test,
    lcm_test,
    fisher_test,
    run_discontinuity_test,
    cox_shi_test,
    skipped_result
  ],
  artma / calc / methods / maive[maive],
  artma / libs / core / utils[get_verbosity]
)

#' @title Extract the skip reason attached to a `skipped_result()` value
#' @param value *[numeric]* A test result, possibly a `skipped_result()`.
#' @return *[character or NULL]* The reason, or `NULL` if `value` is not a skip.
skip_reason <- function(value) {
  attr(value, "reason")
}

# Caliper tests (Gerber & Malhotra, 2008) ---------------------------------

CALIPER_TAILS <- c("auto", "positive", "negative", "absolute")

#' @title Resolve which tail the caliper should inspect
#' @description
#' The canonical Gerber and Malhotra (2008) caliper is one-sided, applied in
#' the direction the literature has an incentive to push its estimates. Signed
#' t-statistics compared against positive-only thresholds therefore inspect the
#' wrong tail whenever the literature is dominated by negative effects, where
#' the reporting incentive sits at `t < -1.96` rather than `t > 1.96`.
#'
#' `"auto"` (the default) picks the tail carrying the majority of the
#' t-statistics. `"positive"` and `"negative"` force one tail; `"absolute"`
#' pools both tails by taking `|t|`, which is the right choice only when the
#' literature genuinely reports both signs as findings.
#' @param t_stats *[numeric]* T-statistics.
#' @param tail *[character]* One of `"auto"`, `"positive"`, `"negative"`, `"absolute"`.
#' @return *[character]* The resolved tail, never `"auto"`.
resolve_caliper_tail <- function(t_stats, tail = "auto") {
  validate(is.character(tail), length(tail) == 1L)
  assert(tail %in% CALIPER_TAILS, paste0("tail must be one of: ", paste(CALIPER_TAILS, collapse = ", ")))

  if (!identical(tail, "auto")) {
    return(tail)
  }

  usable <- t_stats[is.finite(t_stats) & t_stats != 0]
  if (length(usable) == 0) {
    return("positive")
  }

  if (mean(usable > 0) >= 0.5) "positive" else "negative"
}

#' @title Orient t-statistics so the tested tail is the upper one
#' @param t_stats *[numeric]* T-statistics.
#' @param tail *[character]* A resolved tail (not `"auto"`).
#' @return *[numeric]* Oriented t-statistics.
orient_t_stats <- function(t_stats, tail) {
  switch(tail,
    positive = t_stats,
    negative = -t_stats,
    absolute = abs(t_stats),
    t_stats
  )
}

#' @title Cluster-robust test that a window share equals one half
#' @description
#' `stats::binom.test` treats every estimate in the caliper window as an
#' independent draw, which overstates significance when a handful of studies
#' contribute most of the window (a single study reporting eight estimates just
#' above the threshold is one decision, not eight). This computes the same
#' share but tests it with a cluster-robust score statistic: the score residual
#' `y_i - 0.5` is summed within each study, and the variance of the share comes
#' from the between-study spread of those sums, referred to `t(G - 1)`.
#'
#' Falls back to the exact binomial test when clustering is not identified
#' (fewer than two studies, or zero between-study variance).
#' @param above *[logical]* Whether each window observation lies above the threshold.
#' @param study_id *[vector or NULL]* Study identifiers for the window observations.
#' @return *[list]* Contains p_value, n_studies, and cluster_method.
cluster_robust_share_test <- function(above, study_id = NULL) {
  n_total <- length(above)
  n_above <- sum(above)

  exact <- function(n_studies, reason) {
    list(
      p_value = stats::binom.test(n_above, n_total, p = 0.5)$p.value,
      n_studies = n_studies,
      cluster_method = reason
    )
  }

  if (is.null(study_id)) {
    return(exact(NA_integer_, "none"))
  }

  assert(length(study_id) == n_total, "study_id must be the same length as the window observations")

  clusters <- split(above, as.character(study_id))
  n_studies <- length(clusters)
  if (n_studies < 2L) {
    return(exact(n_studies, "none"))
  }

  # Score residuals under H0: share = 0.5, summed within each study.
  cluster_scores <- vapply(clusters, function(y) sum(y - 0.5), numeric(1))
  variance <- (n_studies / (n_studies - 1)) * sum(cluster_scores^2) / n_total^2

  if (!is.finite(variance) || variance <= 0) {
    return(exact(n_studies, "none"))
  }

  z <- ((n_above / n_total) - 0.5) / sqrt(variance)

  list(
    p_value = 2 * stats::pt(-abs(z), df = n_studies - 1L),
    n_studies = n_studies,
    cluster_method = "study"
  )
}

#' @title Describe which side of the threshold carries the excess mass
#' @param share_above *[numeric]* Share of window observations above the threshold.
#' @return *[character]* `"above"`, `"below"`, `"balanced"`, or `NA`.
caliper_direction <- function(share_above) {
  if (!is.finite(share_above)) {
    return(NA_character_)
  }
  if (share_above > 0.5) {
    "above"
  } else if (share_above < 0.5) {
    "below"
  } else {
    "balanced"
  }
}

#' @title Run single Caliper test
#' @description
#' Performs the Gerber and Malhotra (2008) Caliper test to detect selective
#' reporting around a significance threshold: within a narrow window around
#' the threshold, the share of observations just above the threshold is tested
#' against 50%. The window is taken on t-statistics oriented towards the tail
#' the literature has an incentive to report (see [resolve_caliper_tail()]),
#' and the share is tested with a study-clustered statistic when `study_id` is
#' supplied (see [cluster_robust_share_test()]).
#' @param t_stats *[numeric]* T-statistics.
#' @param threshold *[numeric]* T-statistic threshold (default 1.96).
#' @param width *[numeric]* Caliper interval width (default 0.05).
#' @param study_id *[vector, optional]* Study identifiers, one per element of `t_stats`.
#' @param tail *[character, optional]* Tail to inspect; see [resolve_caliper_tail()].
#' @return *[list]* Contains share_above, p_value, n_above, n_below, n_studies,
#'   direction, tail, and cluster_method.
run_single_caliper <- function(t_stats, threshold = 1.96, width = 0.05,
                               study_id = NULL, tail = "auto") {
  validate(
    is.numeric(t_stats),
    is.numeric(threshold),
    is.numeric(width)
  )
  assert(
    is.null(study_id) || length(study_id) == length(t_stats),
    "study_id must be the same length as t_stats"
  )

  resolved_tail <- resolve_caliper_tail(t_stats, tail)
  oriented <- orient_t_stats(t_stats, resolved_tail)

  # Subset to caliper interval
  lower_bound <- oriented > (threshold - width)
  upper_bound <- oriented < (threshold + width)
  in_interval <- lower_bound & upper_bound
  in_interval[is.na(in_interval)] <- FALSE

  window <- oriented[in_interval]
  above <- window > threshold
  n_above <- sum(above)
  n_below <- sum(window < threshold)
  n_total <- n_above + n_below

  if (n_total == 0) {
    return(list(
      share_above = NA_real_,
      p_value = NA_real_,
      n_above = 0,
      n_below = 0,
      n_studies = 0L,
      direction = NA_character_,
      tail = resolved_tail,
      cluster_method = "none"
    ))
  }

  window_studies <- if (is.null(study_id)) NULL else study_id[in_interval]
  test <- cluster_robust_share_test(above, window_studies)
  share_above <- n_above / n_total

  list(
    share_above = share_above,
    p_value = test$p_value,
    n_above = n_above,
    n_below = n_below,
    n_studies = test$n_studies,
    direction = caliper_direction(share_above),
    tail = resolved_tail,
    cluster_method = test$cluster_method
  )
}

#' @title Run Caliper tests across multiple thresholds and widths
#' @param t_stats *[numeric]* T-statistics.
#' @param thresholds *[numeric]* Vector of thresholds to test.
#' @param widths *[numeric]* Vector of caliper widths to test.
#' @param study_id *[vector, optional]* Study identifiers, one per element of `t_stats`.
#' @param tail *[character, optional]* Tail to inspect; see [resolve_caliper_tail()].
#' @param add_significance_marks *[logical]* Whether to add significance marks.
#' @param round_to *[integer]* Number of decimal places.
#' @param show_progress *[logical]* Whether to show progress indicator.
#' @return *[data.frame]* Caliper test results.
run_caliper_tests <- function(t_stats, thresholds = c(1.645, 1.96, 2.58),
                              widths = c(0.05, 0.1, 0.2), study_id = NULL,
                              tail = "auto",
                              add_significance_marks = TRUE, round_to = 3L,
                              show_progress = TRUE) {
  validate(
    is.numeric(t_stats),
    is.numeric(thresholds),
    is.numeric(widths)
  )

  # Dedupe so each threshold-width pair is computed (and later looked up) once
  thresholds <- unique(thresholds)
  widths <- unique(widths)

  results <- list()
  total_tests <- length(thresholds) * length(widths)

  # Show progress bar if requested and verbosity allows
  verbosity <- getOption("artma.verbose", 3)
  show_pb <- show_progress && verbosity >= 3 && total_tests >= 3

  if (show_pb) {
    cli::cli_progress_bar(
      "Computing {total_tests} test{?s}",
      total = total_tests,
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
    )
  }

  resolved_tail <- resolve_caliper_tail(t_stats, tail)

  for (thresh in thresholds) {
    for (w in widths) {
      res <- run_single_caliper(t_stats, thresh, w, study_id = study_id, tail = resolved_tail)

      if (show_pb) {
        cli::cli_progress_update()
      }

      share_formatted <- if (is.finite(res$share_above)) {
        format_number(res$share_above, round_to)
      } else {
        NA_character_
      }

      # Only a share above 0.5 is evidence of p-hacking. A significantly *low*
      # share points the other way, so it must not be starred as if it were a
      # discontinuity in the hacking direction.
      p_value_formatted <- if (is.finite(res$p_value)) {
        p_str <- format_number(res$p_value, round_to)
        starrable <- add_significance_marks && identical(res$direction, "above")
        if (starrable) paste0(p_str, significance_mark(res$p_value)) else p_str
      } else {
        NA_character_
      }

      results[[length(results) + 1]] <- list(
        threshold = thresh,
        width = w,
        share_above = share_formatted,
        p_value = p_value_formatted,
        n_above = res$n_above,
        n_below = res$n_below,
        n_studies = res$n_studies,
        direction = res$direction,
        tail = res$tail,
        cluster_method = res$cluster_method
      )
    }
  }

  if (show_pb) {
    cli::cli_progress_done()
  }

  results
}

#' @title Build caliper summary table
#' @param caliper_results *[list]* List of caliper test results.
#' @param options *[list]* Options containing display_ratios flag.
#' @return *[data.frame]* Formatted caliper summary.
build_caliper_summary <- function(caliper_results, options) {
  if (length(caliper_results) == 0) {
    return(data.frame())
  }

  # Extract unique thresholds and widths
  thresholds <- unique(vapply(caliper_results, function(x) x$threshold, numeric(1)))
  widths <- unique(vapply(caliper_results, function(x) x$width, numeric(1)))

  # Sort them
  thresholds <- sort(thresholds)
  widths <- sort(widths)

  # Get display option
  display_ratios <- options$display_ratios %||% TRUE

  # Initialize result matrix
  n_rows <- length(widths) * 4 # 4 rows per width: share above, direction, p-value, n count
  result <- matrix("", nrow = n_rows, ncol = length(thresholds) + 1)

  # Column names
  col_names <- c("", paste("Threshold", thresholds))

  # Build matrix
  row_idx <- 1
  for (w in widths) {
    # Row 1: Share above the threshold
    result[row_idx, 1] <- paste0("Caliper width ", w, " - Share above")
    # Row 2: Which side of the threshold carries the excess mass
    result[row_idx + 1, 1] <- paste0("Caliper width ", w, " - Direction")
    # Row 3: Test p-value
    result[row_idx + 2, 1] <- paste0("Caliper width ", w, " - p-value")
    # Row 4: n count (ratio or total)
    result[row_idx + 3, 1] <- paste0("Caliper width ", w, if (display_ratios) " - n above/below" else " - n total")

    for (col_idx in seq_along(thresholds)) {
      thresh <- thresholds[col_idx]
      # Find matching result (thresholds/widths are deduped upstream, so exactly one match)
      res <- caliper_results[[which(
        vapply(caliper_results, function(x) x$width == w && x$threshold == thresh, logical(1))
      )]]

      result[row_idx, col_idx + 1] <- res$share_above
      result[row_idx + 1, col_idx + 1] <- caliper_direction_label(res$direction)
      result[row_idx + 2, col_idx + 1] <- res$p_value
      # Display ratio or total based on option
      counts <- if (display_ratios) {
        paste0(res$n_above, "/", res$n_below)
      } else {
        as.character(res$n_above + res$n_below)
      }
      n_studies <- res$n_studies
      result[row_idx + 3, col_idx + 1] <- if (is.null(n_studies) || !is.finite(n_studies) || n_studies == 0) {
        counts
      } else {
        paste0(counts, " (", n_studies, " stud", if (n_studies == 1L) "y" else "ies", ")")
      }
    }

    row_idx <- row_idx + 4
  }

  # Convert to data frame
  df <- as.data.frame(result, stringsAsFactors = FALSE)
  colnames(df) <- col_names

  # Carried for the printed header; dropped by the CSV exporter.
  attr(df, "caliper_tail") <- caliper_results[[1]]$tail
  attr(df, "caliper_cluster_method") <- caliper_results[[1]]$cluster_method

  df
}

#' @title Human-readable label for a caliper direction
#' @param direction *[character]* Value returned by [caliper_direction()].
#' @return *[character]* Label for display.
caliper_direction_label <- function(direction) {
  if (is.null(direction) || is.na(direction)) {
    return("")
  }
  switch(direction,
    above = "excess above",
    below = "excess below (anti-hacking)",
    balanced = "balanced",
    ""
  )
}

# MAIVE utilities ----------------------------------------------------------

# Stock-Yogo rule of thumb: a first-stage F below this leaves the instrumented
# SE too noisy to identify the MAIVE intercept.
MAIVE_WEAK_F_THRESHOLD <- 10

#' @title Prepare data for MAIVE
#' @param df *[data.frame]* Input data with effect, se, n_obs, study_id.
#' @return *[data.frame]* Data formatted for MAIVE (bs, sebs, Ns, studyid).
prepare_maive_data <- function(df) {
  validate(is.data.frame(df))

  required_cols <- c("effect", "se", "n_obs")
  validate(all(required_cols %in% colnames(df)))

  maive_data <- data.frame(
    bs = df$effect,
    sebs = df$se,
    Ns = df$n_obs,
    stringsAsFactors = FALSE
  )

  if ("study_id" %in% colnames(df)) {
    maive_data$studyid <- df$study_id
  }

  maive_data
}

#' @title Format MAIVE results
#' @param maive_output *[list]* Output from maive() function.
#' @param options *[list]* Options.
#' @return *[data.frame]* Formatted MAIVE summary.
format_maive_results <- function(maive_output, options) {
  if (is.null(maive_output)) {
    return(data.frame(
      Statistic = "Error",
      Value = "MAIVE estimation failed",
      stringsAsFactors = FALSE
    ))
  }

  rd <- options$round_to
  rows <- list()
  self_env <- environment()
  add <- function(stat, val) {
    self_env$rows[[length(self_env$rows) + 1L]] <- list(stat = stat, val = val)
  }
  sep <- function() add("", "")

  # --- Estimates ---
  beta_p <- maive_p_from_coef(maive_output$beta, maive_output$SE)
  add("MAIVE coefficient", paste0(format_number(maive_output$beta, rd), significance_mark(beta_p)))
  add("  Std. error", format_se(maive_output$SE, rd))

  selected <- maive_output$petpeese_selected
  if (!is.null(selected) && !is.na(selected)) {
    add("Model selected", selected)
  }

  # --- Publication bias ---
  sep()
  pub_p <- maive_output$`pub bias p-value`
  if (is.numeric(pub_p) && is.finite(pub_p)) {
    add("Pub. bias p-value", paste0(format_number(pub_p, rd), significance_mark(pub_p)))
  } else {
    add("Pub. bias p-value", "NA")
  }

  add("Egger coefficient", format_number(maive_output$egger_coef, rd))
  add("  Std. error", format_se(maive_output$egger_se, rd))

  boot_ci <- maive_output$egger_boot_ci
  if (is.numeric(boot_ci) && length(boot_ci) == 2L && all(is.finite(boot_ci))) {
    add("  95% CI (bootstrap)", format_ci(boot_ci[1L], boot_ci[2L], rd))
  }

  # PEESE SE^2 details (only when PEESE is the selected model)
  peese_coef <- maive_output$peese_se2_coef
  if (is.numeric(peese_coef) && is.finite(peese_coef)) {
    add("PEESE SE^2 coefficient", format_number(peese_coef, rd))
    peese_se <- maive_output$peese_se2_se
    if (is.numeric(peese_se) && is.finite(peese_se)) {
      add("  Std. error", format_se(peese_se, rd))
    }
  }

  # --- Diagnostics ---
  sep()
  ftest <- maive_first_stage_f(maive_output)
  ftest_val <- if (is.na(ftest)) "NA" else format_number(ftest, rd)
  add("F-test (1st stage IV)", ftest_val)
  if (maive_first_stage_is_weak(ftest)) {
    add("  Instrument strength", sprintf("weak (F < %s)", MAIVE_WEAK_F_THRESHOLD))
  }

  add("Hausman test", format_number(maive_output$Hausman, rd))
  add("  Chi-sq. crit. (alpha=0.05)", format_number(maive_output$Chi2, rd))

  ar_ci <- maive_output$AR_CI
  if (is.numeric(ar_ci) && length(ar_ci) == 2L && all(is.finite(ar_ci))) {
    add("AR 95% CI", format_ci(ar_ci[1L], ar_ci[2L], rd))
  }

  # Build data frame
  stats <- vapply(rows, `[[`, "", "stat")
  vals <- vapply(rows, `[[`, "", "val")

  data.frame(
    Statistic = stats,
    Value = vals,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' @title Extract the first-stage F statistic from MAIVE output
#' @description MAIVE reports the statistic as the literal string "NA" when
#'   instrumenting is switched off, so a plain `as.numeric()` is not enough.
#' @param maive_output *[list]* Output from `maive()`.
#' @return *[numeric]* The F statistic, or `NA_real_` when unavailable.
maive_first_stage_f <- function(maive_output) {
  raw <- maive_output$`F-test`
  if (is.null(raw) || length(raw) != 1L || identical(raw, "NA")) {
    return(NA_real_)
  }
  val <- suppressWarnings(as.numeric(raw))
  if (!is.finite(val)) NA_real_ else val
}

#' @title Flag a weak MAIVE first stage
#' @param ftest *[numeric]* First-stage F statistic.
#' @return *[logical]* TRUE when the instrument is weak by the Stock-Yogo rule of thumb.
maive_first_stage_is_weak <- function(ftest) {
  is.numeric(ftest) && length(ftest) == 1L && is.finite(ftest) && ftest < MAIVE_WEAK_F_THRESHOLD
}

#' @title Compute two-sided p-value from coefficient and SE
#' @param beta *[numeric]* Coefficient.
#' @param se *[numeric]* Standard error.
#' @return *[numeric]* P-value, or NA if inputs are not finite.
maive_p_from_coef <- function(beta, se) {
  if (!is.numeric(beta) || !is.numeric(se) || !is.finite(beta) || !is.finite(se) || se <= 0) {
    return(NA_real_)
  }
  2 * stats::pnorm(-abs(beta / se))
}

# P-value utilities --------------------------------------------------------

#' @title Compute p-values from effect and se
#' @description
#' Calculate two-sided p-values from effect estimates and standard errors.
#' @param effect *[numeric]* Effect estimates.
#' @param se *[numeric]* Standard errors.
#' @return *[numeric]* Vector of p-values.
compute_pvalues <- function(effect, se) {
  validate(
    is.numeric(effect),
    is.numeric(se),
    length(effect) == length(se)
  )

  t_stats <- effect / se
  2 * stats::pnorm(-abs(t_stats))
}

#' @title Resolve t-statistics, preferring a user-mapped column
#' @description
#' Uses `df$t_stat` where reported, which is more faithful to what the primary
#' study published (e.g. clustered standard errors the meta-analyst cannot
#' recompute from `effect`/`se` alone), falling back to `effect / se` for rows
#' where `t_stat` is absent or `NA`.
#' @param df *[data.frame]* Input data with effect, se, and optional t_stat columns.
#' @return *[numeric]* Vector of t-statistics, one per row of `df`.
resolve_t_stats <- function(df) {
  validate(is.data.frame(df), is.numeric(df$effect), is.numeric(df$se))

  fallback <- df$effect / df$se
  if (!"t_stat" %in% colnames(df)) {
    return(fallback)
  }

  reported <- suppressWarnings(as.numeric(df$t_stat))
  missing <- is.na(reported)
  reported[missing] <- fallback[missing]
  reported
}

#' @title Run binomial test wrapper
#' @param pvalues *[numeric]* P-values to test.
#' @param p_min *[numeric]* Lower bound of interval.
#' @param p_max *[numeric]* Upper bound of interval.
#' @param type *[character]* Test type ("c" for closed, "o" for open).
#' @return *[numeric]* P-value from test.
run_binomial <- function(pvalues, p_min, p_max, type = "c") {
  validate(
    is.numeric(pvalues),
    is.numeric(p_min),
    is.numeric(p_max),
    is.character(type)
  )

  assert(p_min < p_max, "p_min must be less than p_max")
  assert(type %in% c("c", "o"), "type must be 'c' or 'o'")

  tryCatch(
    binomial_test(pvalues, p_min, p_max, type),
    error = function(e) skipped_result(e$message)
  )
}

#' @title Run LCM test wrapper
#' @param pvalues *[numeric]* P-values to test.
#' @param p_min *[numeric]* Lower bound of interval.
#' @param p_max *[numeric]* Upper bound of interval.
#' @param cdfs *[numeric]* Simulated CDFs for critical values.
#' @return *[numeric]* P-value from test.
run_lcm <- function(pvalues, p_min, p_max, cdfs) {
  validate(
    is.numeric(pvalues),
    is.numeric(p_min),
    is.numeric(p_max),
    is.numeric(cdfs)
  )

  assert(p_min < p_max, "p_min must be less than p_max")

  if (!requireNamespace("fdrtool", quietly = TRUE)) {
    return(skipped_result("package 'fdrtool' is not installed"))
  }

  tryCatch(
    lcm_test(pvalues, p_min, p_max, cdfs),
    error = function(e) skipped_result(e$message)
  )
}

#' @title Run Fisher test wrapper
#' @param pvalues *[numeric]* P-values to test.
#' @param p_min *[numeric]* Lower bound of interval.
#' @param p_max *[numeric]* Upper bound of interval.
#' @return *[numeric]* P-value from test.
run_fisher <- function(pvalues, p_min, p_max) {
  validate(
    is.numeric(pvalues),
    is.numeric(p_min),
    is.numeric(p_max)
  )

  assert(p_min < p_max, "p_min must be less than p_max")

  tryCatch(
    fisher_test(pvalues, p_min, p_max),
    error = function(e) skipped_result(e$message)
  )
}

#' @title Run discontinuity test wrapper
#' @param pvalues *[numeric]* P-values to test.
#' @param cutoff *[numeric]* Significance threshold to test for discontinuity.
#' @param bandwidth *[numeric, optional]* Manual bandwidth override. Leave
#'   `NULL` to let rddensity select its bandwidth automatically.
#' @return *[numeric]* P-value from test.
run_discontinuity <- function(pvalues, cutoff = 0.05, bandwidth = NULL) {
  validate(
    is.numeric(pvalues),
    is.numeric(cutoff)
  )

  assert(cutoff > 0 && cutoff < 1, "cutoff must be in (0, 1)")

  if (!is.null(bandwidth)) {
    validate(is.numeric(bandwidth))
    assert(bandwidth > 0, "bandwidth must be positive")
  }

  if (!requireNamespace("rddensity", quietly = TRUE)) {
    return(skipped_result("package 'rddensity' is not installed"))
  }

  tryCatch(
    run_discontinuity_test(pvalues, c = cutoff, h = bandwidth),
    error = function(e) skipped_result(e$message)
  )
}

#' @title Run Cox-Shi test wrapper
#' @description
#' `n_bins` fixes the bin *count*, not the bin *width*, so the same value
#' applied to the [0, 0.05] and [0, 0.10] windows makes the wider window twice
#' as coarse. A spike in the p-curve narrower than one bin can be averaged away
#' on the wider window while remaining visible on the narrower one, and the two
#' windows are then not comparable, nor need their p-values be ordered. Compare
#' windows at a matched bin width (double `n_bins` when doubling `p_max`)
#' before reading anything into a disagreement between them.
#' @param pvalues *[numeric]* P-values to test.
#' @param study_id *[vector]* Study identifiers for clustering.
#' @param p_min *[numeric]* Lower bound of interval.
#' @param p_max *[numeric]* Upper bound of interval.
#' @param n_bins *[integer]* Number of bins for discretization.
#' @param monotonicity_order *[integer]* Order of monotonicity (K).
#' @param use_bounds *[integer]* Whether to use bounds (0 or 1).
#' @return *[numeric]* P-value from test.
run_cox_shi <- function(pvalues, study_id = NULL, p_min, p_max, n_bins = 10L,
                        monotonicity_order = 2L, use_bounds = 1L) {
  validate(
    is.numeric(pvalues),
    is.numeric(p_min),
    is.numeric(p_max),
    is.numeric(n_bins),
    is.numeric(monotonicity_order),
    is.numeric(use_bounds)
  )

  assert(p_min < p_max, "p_min must be less than p_max")
  assert(n_bins > 0, "n_bins must be positive")
  assert(monotonicity_order >= 0, "monotonicity_order must be non-negative")
  assert(use_bounds %in% c(0, 1), "use_bounds must be 0 or 1")

  if (is.null(study_id)) {
    study_id <- seq_along(pvalues)
  }

  result <- tryCatch(
    cox_shi_test(
      Q = pvalues,
      ind = study_id,
      p_min = p_min,
      p_max = p_max,
      J = as.integer(n_bins),
      K = as.integer(monotonicity_order),
      use_bounds = as.integer(use_bounds)
    ),
    error = function(e) skipped_result(e$message)
  )

  result
}

#' @title Run suite of p-hacking tests
#' @description
#' Executes comprehensive p-hacking detection tests: Caliper (Gerber & Malhotra, 2008),
#' Elliott et al. (2022), and MAIVE (Irsova et al., 2023).
#' @param df *[data.frame]* Input data with effect, se, study_id, n_obs columns.
#' @param options *[list]* Options containing test parameters.
#' @return *[list]* Contains test results and formatted summaries.
run_p_hacking_tests <- function(df, options) {
  validate(is.data.frame(df), is.list(options))

  study_id <- if ("study_id" %in% colnames(df)) df$study_id else seq_len(nrow(df))
  t_stats <- resolve_t_stats(df)
  pvalues <- 2 * stats::pnorm(-abs(t_stats))

  output <- list()
  skipped <- list()
  record_skip <- function(key, value) {
    reason <- skip_reason(value)
    if (!is.null(reason)) {
      skipped[[key]] <<- reason
    }
    invisible(NULL)
  }

  # 1. Caliper Tests
  if (options$include_caliper) {
    caliper_results <- tryCatch(
      {
        run_caliper_tests(
          t_stats = t_stats,
          thresholds = options$caliper_thresholds,
          widths = options$caliper_widths,
          study_id = if (isFALSE(options$caliper_cluster)) NULL else study_id,
          tail = options$caliper_tail %||% "auto",
          add_significance_marks = options$add_significance_marks,
          round_to = options$round_to
        )
      },
      error = function(e) {
        cli::cli_warn("Caliper tests failed: {e$message}")
        list()
      }
    )
    output$caliper <- build_caliper_summary(caliper_results, list(display_ratios = options$caliper_display_ratios))
  }

  # 2. Elliott Tests
  if (options$include_elliott) {
    elliott_tests <- list()

    # Pre-simulate CDFs for LCM test
    cdfs_reason <- NULL
    cdfs <- tryCatch(
      simulate_cdfs_parallel(
        iterations = options$lcm_iterations,
        grid_points = options$lcm_grid_points,
        block_size = options$simulate_cdfs_chunk_size,
        seed = options$simulate_cdfs_seed
      ),
      error = function(e) {
        cdfs_reason <<- e$message
        numeric(0)
      }
    )

    # Binomial tests
    elliott_tests$binomial_005 <- list(
      test = "Binomial [0, 0.05]",
      p_value = run_binomial(pvalues, 0, 0.05, type = "c")
    )

    elliott_tests$binomial_01 <- list(
      test = "Binomial [0, 0.10]",
      p_value = run_binomial(pvalues, 0, 0.1, type = "c")
    )

    # LCM tests (always reported, even when the CDF simulation failed, so the
    # skip reason surfaces instead of the rows silently disappearing)
    if (length(cdfs) > 0) {
      elliott_tests$lcm_005 <- list(
        test = "LCM [0, 0.05]",
        p_value = run_lcm(pvalues, 0, 0.05, cdfs)
      )

      elliott_tests$lcm_01 <- list(
        test = "LCM [0, 0.10]",
        p_value = run_lcm(pvalues, 0, 0.1, cdfs)
      )
    } else {
      lcm_skip <- skipped_result(cdfs_reason %||% "CDF simulation returned no draws")
      elliott_tests$lcm_005 <- list(test = "LCM [0, 0.05]", p_value = lcm_skip)
      elliott_tests$lcm_01 <- list(test = "LCM [0, 0.10]", p_value = lcm_skip)
    }

    # Fisher tests
    elliott_tests$fisher_005 <- list(
      test = "Fisher [0, 0.05]",
      p_value = run_fisher(pvalues, 0, 0.05)
    )

    elliott_tests$fisher_01 <- list(
      test = "Fisher [0, 0.10]",
      p_value = run_fisher(pvalues, 0, 0.1)
    )

    # Discontinuity test
    if (options$include_discontinuity) {
      elliott_tests$discontinuity <- list(
        test = "Discontinuity at 0.05",
        p_value = run_discontinuity(pvalues, cutoff = 0.05, bandwidth = options$discontinuity_bandwidth)
      )
    }

    # Cox-Shi tests
    if (options$include_cox_shi) {
      elliott_tests$cox_shi_005 <- list(
        test = "Cox-Shi [0, 0.05]",
        p_value = run_cox_shi(
          pvalues, study_id, 0, 0.05,
          n_bins = options$cox_shi_bins,
          monotonicity_order = options$cox_shi_order,
          use_bounds = options$cox_shi_bounds
        )
      )

      elliott_tests$cox_shi_01 <- list(
        test = "Cox-Shi [0, 0.10]",
        p_value = run_cox_shi(
          pvalues, study_id, 0, 0.1,
          n_bins = options$cox_shi_bins,
          monotonicity_order = options$cox_shi_order,
          use_bounds = options$cox_shi_bounds
        )
      )
    }

    for (key in names(elliott_tests)) {
      record_skip(key, elliott_tests[[key]]$p_value)
    }

    output$elliott <- build_elliott_summary(elliott_tests, pvalues, options)
  }

  # 3. MAIVE Estimator
  if (options$include_maive) {
    if ("n_obs" %in% colnames(df)) {
      maive_data <- prepare_maive_data(df)

      if (options$maive_se == 3L) {
        cli::cli_alert_info("Running MAIVE with wild cluster bootstrap (this may take a moment)...")
      }

      maive_results <- tryCatch(
        {
          maive(
            dat = maive_data,
            method = options$maive_method,
            weight = options$maive_weight,
            instrument = options$maive_instrument,
            studylevel = options$maive_studylevel,
            SE = options$maive_se,
            AR = options$maive_ar,
            first_stage = options$maive_first_stage,
            seed = options$maive_seed
          )
        },
        error = function(e) {
          # conditionMessage(), not e$message: the latter keeps only the cli
          # header, dropping the bullets that carry the install and upgrade
          # hints the user needs to act on the skip.
          skipped[["maive"]] <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(maive_results) && get_verbosity() >= 2) {
        maive_f <- maive_first_stage_f(maive_results)
        if (maive_first_stage_is_weak(maive_f)) {
          cli::cli_alert_warning(c(
            "MAIVE first-stage F is {round(maive_f, 3)}, below {MAIVE_WEAK_F_THRESHOLD}: ",
            "the sample-size instrument is weak and the MAIVE estimate is unreliable."
          ))
          cli::cli_alert_info(
            "Try {.code maive_first_stage: 1} (log first stage), which fits data whose sample sizes span orders of magnitude."
          )
        }
      }

      output$maive <- if (is.null(maive_results)) {
        NULL
      } else {
        tryCatch(
          format_maive_results(maive_results, options),
          error = function(e) {
            skipped[["maive"]] <<- paste("MAIVE result formatting failed:", e$message)
            NULL
          }
        )
      }
    } else {
      skipped[["maive"]] <- "requires the 'n_obs' column, which is absent from the data"
      output$maive <- NULL
    }
  }

  output$n_pvalues <- length(pvalues)
  output$n_significant_005 <- sum(pvalues <= 0.05, na.rm = TRUE)
  output$n_significant_010 <- sum(pvalues <= 0.10, na.rm = TRUE)
  output$options <- options
  output$skipped <- if (length(skipped) > 0) skipped else NULL

  output
}

#' @title Build Elliott tests summary table
#' @param elliott_tests *[list]* Elliott test results.
#' @param pvalues *[numeric]* P-values for counts.
#' @param options *[list]* Options.
#' @return *[data.frame]* Formatted Elliott summary table.
build_elliott_summary <- function(elliott_tests, pvalues, options) {
  test_names <- vapply(elliott_tests, function(x) x$test, character(1))
  p_vals <- vapply(elliott_tests, function(x) x$p_value, numeric(1))

  # Format p-values
  p_values_formatted <- format_number(p_vals, options$round_to)

  # Add significance markers
  p_values_formatted <- paste0(p_values_formatted, significance_mark(p_vals))

  summary <- data.frame(
    Test = test_names,
    `P-value` = p_values_formatted,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add observation count rows
  n_01_range <- sum(pvalues >= 0 & pvalues <= 0.1, na.rm = TRUE)
  n_005_range <- sum(pvalues >= 0 & pvalues <= 0.05, na.rm = TRUE)

  obs_rows <- data.frame(
    Test = c("Observations in [0, 0.1]", "Observations in [0, 0.05]"),
    `P-value` = c(as.character(n_01_range), as.character(n_005_range)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  summary <- rbind(summary, obs_rows)
  rownames(summary) <- NULL
  summary
}

box::export(
  run_caliper_tests,
  run_p_hacking_tests,
  run_single_caliper,
  resolve_caliper_tail,
  cluster_robust_share_test,
  caliper_direction,
  build_caliper_summary,
  compute_pvalues,
  resolve_t_stats,
  prepare_maive_data,
  maive_p_from_coef,
  maive_first_stage_f,
  maive_first_stage_is_weak,
  format_maive_results,
  run_binomial,
  run_lcm,
  run_discontinuity,
  run_cox_shi,
  run_fisher
)
