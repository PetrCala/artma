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
  model_coefs <- tryCatch(
    {
      lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "const", cluster = df_subset$study))
    },
    error = function(e) {
      lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "const"))
    }
  )

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
#' @param show_progress *[logical]* Whether to show progress indicator.
#' @return *[data.frame]* Caliper test results.
run_caliper_tests <- function(t_stats, study_id, thresholds = c(0, 1.96, 2.58),
                              widths = c(0.05, 0.1, 0.2),
                              add_significance_marks = TRUE, round_to = 3L,
                              show_progress = TRUE) {
  validate(
    is.numeric(t_stats),
    is.numeric(thresholds),
    is.numeric(widths)
  )

  results <- list()
  total_tests <- length(thresholds) * length(widths)

  # Show progress bar if requested and verbosity allows
  verbosity <- getOption("artma.verbose", 3)
  show_pb <- show_progress && verbosity >= 3 && total_tests >= 3

  if (show_pb) {
    cli::cli_progress_bar(
      "Running Caliper tests",
      total = total_tests,
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} tests [{cli::pb_elapsed}]"
    )
  }

  for (thresh in thresholds) {
    for (w in widths) {
      res <- run_single_caliper(t_stats, study_id, thresh, w, add_significance_marks)

      if (show_pb) {
        cli::cli_progress_update()
      }

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
  n_rows <- length(widths) * 3 # 3 rows per width: Estimate, SE, n count
  result <- matrix("", nrow = n_rows, ncol = length(thresholds) + 1)

  # Column names
  col_names <- c("", paste("Threshold", thresholds))

  # Build matrix
  row_idx <- 1
  for (w in widths) {
    # Row 1: Estimates
    result[row_idx, 1] <- paste0("Caliper width ", w, " - Estimate")
    # Row 2: SEs
    result[row_idx + 1, 1] <- paste0("Caliper width ", w, " - SE")
    # Row 3: n count (ratio or total)
    result[row_idx + 2, 1] <- paste0("Caliper width ", w, if (display_ratios) " - n above/below" else " - n total")

    for (col_idx in seq_along(thresholds)) {
      thresh <- thresholds[col_idx]
      # Find matching result
      res <- caliper_results[[which(
        vapply(caliper_results, function(x) x$width == w && x$threshold == thresh, logical(1))
      )]]

      result[row_idx, col_idx + 1] <- res$estimate
      result[row_idx + 1, col_idx + 1] <- res$std_error
      # Display ratio or total based on option
      result[row_idx + 2, col_idx + 1] <- if (display_ratios) {
        paste0(res$n_above, "/", res$n_below)
      } else {
        as.character(res$n_above + res$n_below)
      }
    }

    row_idx <- row_idx + 3
  }

  # Convert to data frame
  df <- as.data.frame(result, stringsAsFactors = FALSE)
  colnames(df) <- col_names

  df
}

# MAIVE utilities ----------------------------------------------------------

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

  beta_formatted <- format_number(maive_output$beta, options$round_to)
  se_formatted <- format_number(maive_output$SE, options$round_to)
  ftest_formatted <- if (!is.null(maive_output$`F-test`) && maive_output$`F-test` != "NA") {
    format_number(as.numeric(maive_output$`F-test`), options$round_to)
  } else {
    "NA"
  }
  hausman_formatted <- format_number(maive_output$Hausman, options$round_to)
  chi2_formatted <- format_number(maive_output$Chi2, options$round_to)

  data.frame(
    Statistic = c(
      "MAIVE Coefficient",
      "MAIVE SE",
      "F-test (1st stage IV)",
      "Hausman Test",
      "Chi-squared Critical (α=0.05)"
    ),
    Value = c(
      beta_formatted,
      se_formatted,
      ftest_formatted,
      hausman_formatted,
      chi2_formatted
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
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
#' Executes comprehensive p-hacking detection tests: Caliper (Gerber & Malhotra, 2008),
#' Elliott et al. (2022), and MAIVE (Irsova et al., 2023).
#' @param df *[data.frame]* Input data with effect, se, study_id, n_obs columns.
#' @param options *[list]* Options containing test parameters.
#' @return *[list]* Contains test results and formatted summaries.
run_p_hacking_tests <- function(df, options) {
  validate(is.data.frame(df), is.list(options))

  required_cols <- c("effect", "se")
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    cli::cli_alert_warning("Missing required columns: {.field {missing_cols}}")
    return(list(
      caliper = NULL,
      elliott = NULL,
      maive = NULL,
      skipped = list(reason = paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    ))
  }

  study_id <- if ("study_id" %in% colnames(df)) df$study_id else seq_len(nrow(df))
  t_stats <- df$effect / df$se
  pvalues <- compute_pvalues(df$effect, df$se)

  output <- list()

  # 1. Caliper Tests
  if (options$include_caliper) {
    caliper_results <- tryCatch(
      {
        run_caliper_tests(
          t_stats = t_stats,
          study_id = study_id,
          thresholds = options$caliper_thresholds,
          widths = options$caliper_widths,
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
    cdfs <- tryCatch(
      simulate_cdfs(iterations = options$lcm_iterations, grid_points = options$lcm_grid_points),
      error = function(e) {
        cli::cli_warn("Failed to simulate CDFs for LCM test: {e$message}")
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

    # LCM tests
    if (length(cdfs) > 0) {
      elliott_tests$lcm_005 <- list(
        test = "LCM [0, 0.05]",
        p_value = run_lcm(pvalues, 0, 0.05, cdfs)
      )

      elliott_tests$lcm_01 <- list(
        test = "LCM [0, 0.10]",
        p_value = run_lcm(pvalues, 0, 0.1, cdfs)
      )
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

    output$elliott <- build_elliott_summary(elliott_tests, pvalues, options)
  }

  # 3. MAIVE Estimator
  if (options$include_maive) {
    if ("n_obs" %in% colnames(df)) {
      maive_data <- prepare_maive_data(df)
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
            first_stage = options$maive_first_stage
          )
        },
        error = function(e) {
          cli::cli_warn("MAIVE estimation failed: {e$message}")
          NULL
        }
      )
      output$maive <- format_maive_results(maive_results, options)
    } else {
      cli::cli_warn("MAIVE requires 'n_obs' column - skipping MAIVE test")
      output$maive <- NULL
    }
  }

  output$n_pvalues <- length(pvalues)
  output$n_significant_005 <- sum(pvalues <= 0.05, na.rm = TRUE)
  output$n_significant_010 <- sum(pvalues <= 0.10, na.rm = TRUE)
  output$options <- options

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
  significance <- character(length(p_vals))
  for (i in seq_along(p_vals)) {
    if (is.na(p_vals[i])) {
      significance[i] <- ""
    } else if (p_vals[i] <= 0.01) {
      significance[i] <- "***"
    } else if (p_vals[i] <= 0.05) {
      significance[i] <- "**"
    } else if (p_vals[i] <= 0.10) {
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

  # Add observation count rows
  n_total <- length(pvalues)
  n_010 <- sum(pvalues <= 0.1, na.rm = TRUE)
  n_005 <- sum(pvalues <= 0.05, na.rm = TRUE)
  n_01_range <- sum(pvalues >= 0 & pvalues <= 0.1, na.rm = TRUE)
  n_005_range <- sum(pvalues >= 0 & pvalues <= 0.05, na.rm = TRUE)

  obs_rows <- data.frame(
    Test = c("Observations in [0, 0.1]", "Observations in [0, 0.05]", "Observations <= 0.1"),
    `P-value` = c(as.character(n_01_range), as.character(n_005_range), as.character(n_010)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  summary <- rbind(summary, obs_rows)
  rownames(summary) <- NULL
  summary
}

box::export(
  run_caliper_tests,
  run_p_hacking_tests
)
