#' @title P-hacking test helpers
#' @description
#' Helper functions for comprehensive p-hacking detection tests.
#' Includes Caliper tests (Gerber & Malhotra, 2008), Elliott tests (2022),
#' and MAIVE estimator (Irsova et al., 2023).
NULL

box::use(
  stats[pchisq, ecdf, lm, coef],
  artma / libs / validation[validate, assert],
  artma / libs / result_formatters[
    format_number,
    significance_mark
  ],
  artma / calc / methods / elliott[
    simulate_cdfs,
    binomial_test,
    lcm_test,
    fisher_test,
    run_discontinuity_test,
    cox_shi_test
  ],
  artma / calc / methods / maive[maive]
)

# Caliper tests (Gerber & Malhotra, 2008) ---------------------------------

#' @title Run single Caliper test
#' @description
#' Performs a Caliper test to detect selective reporting around significance thresholds.
#' @param t_stats *[numeric]* T-statistics.
#' @param study_id *[vector]* Study identifiers for clustering.
#' @param threshold *[numeric]* T-statistic threshold (default 1.96).
#' @param width *[numeric]* Caliper interval width (default 0.05).
#' @param add_significance_marks *[logical]* Whether to add significance asterisks.
#' @return *[list]* Contains estimate, SE, n_above, n_below.
run_single_caliper <- function(t_stats, study_id, threshold = 1.96, width = 0.05, add_significance_marks = TRUE) {
  validate(
    is.numeric(t_stats),
    is.numeric(threshold),
    is.numeric(width)
  )

  # Identify significant observations
  significant_obs <- if (threshold >= 0) {
    t_stats > threshold
  } else {
    t_stats < threshold
  }

  # Subset to caliper interval
  lower_bound <- t_stats > (threshold - width)
  upper_bound <- t_stats < (threshold + width)
  in_interval <- lower_bound & upper_bound

  if (sum(in_interval) == 0) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      n_above = 0,
      n_below = 0
    ))
  }

  # Prepare data for regression
  df_subset <- data.frame(
    significant = as.numeric(significant_obs[in_interval]),
    t_stat = t_stats[in_interval],
    study = study_id[in_interval]
  )

  # Run regression
  model <- stats::lm(significant ~ t_stat - 1, data = df_subset)
  model_coefs <- tryCatch({
    lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "const", cluster = df_subset$study))
  }, error = function(e) {
    lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "const"))
  })

  estimate <- model_coefs["t_stat", "Estimate"]
  std_error <- model_coefs["t_stat", "Std. Error"]
  n_above <- sum(t_stats[in_interval] > threshold)
  n_below <- sum(t_stats[in_interval] < threshold)

  list(
    estimate = estimate,
    std_error = std_error,
    n_above = n_above,
    n_below = n_below
  )
}

#' @title Run Caliper tests across multiple thresholds and widths
#' @param t_stats *[numeric]* T-statistics.
#' @param study_id *[vector]* Study identifiers.
#' @param thresholds *[numeric]* Vector of thresholds to test.
#' @param widths *[numeric]* Vector of caliper widths to test.
#' @param add_significance_marks *[logical]* Whether to add significance marks.
#' @param round_to *[integer]* Number of decimal places.
#' @return *[data.frame]* Caliper test results.
run_caliper_tests <- function(t_stats, study_id, thresholds = c(0, 1.96, 2.58),
                              widths = c(0.05, 0.1, 0.2),
                              add_significance_marks = TRUE, round_to = 3L) {
  validate(
    is.numeric(t_stats),
    is.numeric(thresholds),
    is.numeric(widths)
  )

  results <- list()

  for (thresh in thresholds) {
    for (w in widths) {
      res <- run_single_caliper(t_stats, study_id, thresh, w, add_significance_marks)

      est_formatted <- if (is.finite(res$estimate)) {
        est_str <- format_number(res$estimate, round_to)
        if (add_significance_marks && is.finite(res$std_error)) {
          p_val <- 2 * stats::pnorm(-abs(res$estimate / res$std_error))
          mark <- significance_mark(p_val)
          paste0(est_str, mark)
        } else {
          est_str
        }
      } else {
        NA_character_
      }

      se_formatted <- if (is.finite(res$std_error)) {
        paste0("(", format_number(res$std_error, round_to), ")")
      } else {
        NA_character_
      }

      results[[length(results) + 1]] <- list(
        threshold = thresh,
        width = w,
        estimate = est_formatted,
        std_error = se_formatted,
        n_above = res$n_above,
        n_below = res$n_below
      )
    }
  }

  results
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
    error = function(e) NA_real_
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

  tryCatch(
    lcm_test(pvalues, p_min, p_max, norm = 8, cdfs),
    error = function(e) NA_real_
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
    error = function(e) NA_real_
  )
}

#' @title Run discontinuity test wrapper
#' @param pvalues *[numeric]* P-values to test.
#' @param cutoff *[numeric]* Significance threshold to test for discontinuity.
#' @param bandwidth *[numeric]* Initial bandwidth for test.
#' @return *[numeric]* P-value from test.
run_discontinuity <- function(pvalues, cutoff = 0.05, bandwidth = 0.05) {
  validate(
    is.numeric(pvalues),
    is.numeric(cutoff),
    is.numeric(bandwidth)
  )

  assert(cutoff > 0 && cutoff < 1, "cutoff must be in (0, 1)")
  assert(bandwidth > 0, "bandwidth must be positive")

  if (!requireNamespace("rddensity", quietly = TRUE)) {
    cli::cli_warn("Package 'rddensity' not available - skipping discontinuity test")
    return(NA_real_)
  }

  tryCatch(
    run_discontinuity_test(pvalues, c = cutoff, h = bandwidth),
    error = function(e) NA_real_
  )
}

#' @title Run Cox-Shi test wrapper
#' @param pvalues *[numeric]* P-values to test.
#' @param study_id *[vector]* Study identifiers for clustering.
#' @param p_min *[numeric]* Lower bound of interval.
#' @param p_max *[numeric]* Upper bound of interval.
#' @param n_bins *[integer]* Number of bins for discretization.
#' @param monotonicity_order *[integer]* Order of monotonicity (K).
#' @param use_bounds *[integer]* Whether to use bounds (0 or 1).
#' @return *[numeric]* P-value from test.
run_cox_shi <- function(pvalues, study_id = NULL, p_min, p_max, n_bins = 20L,
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

  tryCatch(
    cox_shi_test(
      Q = pvalues,
      ind = study_id,
      p_min = p_min,
      p_max = p_max,
      J = as.integer(n_bins),
      K = as.integer(monotonicity_order),
      use_bounds = as.integer(use_bounds)
    ),
    error = function(e) NA_real_
  )
}

#' @title Run suite of p-hacking tests
#' @description
#' Executes multiple p-hacking detection tests on a dataset of effect estimates.
#' Based on Elliott, Kudrin & Wüthrich (2022).
#' @param df *[data.frame]* Input data with effect, se, study_id columns.
#' @param options *[list]* Options containing test parameters.
#' @return *[list]* Contains test results and formatted summary.
run_p_hacking_tests <- function(df, options) {
  validate(is.data.frame(df), is.list(options))

  required_cols <- c("effect", "se")
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    cli::cli_alert_warning("Missing required columns: {.field {missing_cols}}")
    return(list(
      results = data.frame(),
      summary = data.frame(),
      skipped = list(reason = paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    ))
  }

  # Compute p-values
  pvalues <- compute_pvalues(df$effect, df$se)
  study_id <- if ("study_id" %in% colnames(df)) df$study_id else seq_len(nrow(df))

  # Pre-simulate CDFs for LCM test
  cdfs <- tryCatch(
    simulate_cdfs(iterations = options$lcm_iterations, grid_points = options$lcm_grid_points),
    error = function(e) {
      cli::cli_warn("Failed to simulate CDFs for LCM test: {e$message}")
      numeric(0)
    }
  )

  # Run tests
  results <- list()

  # Binomial test (0, 0.05]
  results$binomial_005 <- list(
    test = "Binomial [0, 0.05]",
    p_value = run_binomial(pvalues, 0, 0.05, type = "c")
  )

  # Binomial test (0, 0.1]
  results$binomial_01 <- list(
    test = "Binomial [0, 0.10]",
    p_value = run_binomial(pvalues, 0, 0.1, type = "c")
  )

  # LCM test
  if (length(cdfs) > 0) {
    results$lcm_005 <- list(
      test = "LCM [0, 0.05]",
      p_value = run_lcm(pvalues, 0, 0.05, cdfs)
    )

    results$lcm_01 <- list(
      test = "LCM [0, 0.10]",
      p_value = run_lcm(pvalues, 0, 0.1, cdfs)
    )
  }

  # Fisher test
  results$fisher_005 <- list(
    test = "Fisher [0, 0.05]",
    p_value = run_fisher(pvalues, 0, 0.05)
  )

  results$fisher_01 <- list(
    test = "Fisher [0, 0.10]",
    p_value = run_fisher(pvalues, 0, 0.1)
  )

  # Discontinuity test (if rddensity available)
  if (options$include_discontinuity) {
    results$discontinuity <- list(
      test = "Discontinuity at 0.05",
      p_value = run_discontinuity(pvalues, cutoff = 0.05, bandwidth = options$discontinuity_bandwidth)
    )
  }

  # Cox-Shi tests
  if (options$include_cox_shi) {
    results$cox_shi_005 <- list(
      test = "Cox-Shi [0, 0.05]",
      p_value = run_cox_shi(
        pvalues, study_id, 0, 0.05,
        n_bins = options$cox_shi_bins,
        monotonicity_order = options$cox_shi_order,
        use_bounds = options$cox_shi_bounds
      )
    )

    results$cox_shi_01 <- list(
      test = "Cox-Shi [0, 0.10]",
      p_value = run_cox_shi(
        pvalues, study_id, 0, 0.1,
        n_bins = options$cox_shi_bins,
        monotonicity_order = options$cox_shi_order,
        use_bounds = options$cox_shi_bounds
      )
    )
  }

  # Build summary table
  summary <- build_p_hacking_summary(results, options)

  list(
    results = results,
    summary = summary,
    n_pvalues = length(pvalues),
    n_significant_005 = sum(pvalues <= 0.05, na.rm = TRUE),
    n_significant_010 = sum(pvalues <= 0.10, na.rm = TRUE),
    options = options
  )
}

#' @title Build p-hacking tests summary table
#' @param results *[list]* Test results.
#' @param options *[list]* Options.
#' @return *[data.frame]* Formatted summary table.
build_p_hacking_summary <- function(results, options) {
  test_names <- vapply(results, function(x) x$test, character(1))
  p_values <- vapply(results, function(x) x$p_value, numeric(1))

  # Format p-values
  p_values_formatted <- format_number(p_values, options$round_to)

  # Add significance markers
  significance <- character(length(p_values))
  for (i in seq_along(p_values)) {
    if (is.na(p_values[i])) {
      significance[i] <- ""
    } else if (p_values[i] <= 0.01) {
      significance[i] <- "***"
    } else if (p_values[i] <= 0.05) {
      significance[i] <- "**"
    } else if (p_values[i] <= 0.10) {
      significance[i] <- "*"
    } else {
      significance[i] <- ""
    }
  }

  p_values_formatted <- paste0(p_values_formatted, significance)

  summary <- data.frame(
    Test = test_names,
    `P-value` = p_values_formatted,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  rownames(summary) <- NULL
  summary
}

box::export(
  compute_pvalues,
  run_binomial,
  run_lcm,
  run_fisher,
  run_discontinuity,
  run_cox_shi,
  run_p_hacking_tests
)
