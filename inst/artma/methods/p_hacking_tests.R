#' @title P-hacking tests
#' @description
#' Run a comprehensive suite of publication bias tests designed to detect
#' p-hacking and selective reporting. Tests include:
#' - Caliper tests (Gerber & Malhotra, 2008)
#' - Elliott et al. (2022) tests (Binomial, LCM, Fisher, Discontinuity, Cox-Shi)
#'
#' The MAIVE estimator has its own method (`maive`); it is an estimator rather
#' than a test, and its diagnostics do not read as p-hacking p-values.
p_hacking_tests <- function(df) {
  box::use(
    artma / libs / core / validation[validate, validate_columns],
    artma / libs / core / utils[get_verbosity],
    artma / econometric / p_hacking[run_p_hacking_tests],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / options / significance_marks[resolve_add_significance_marks],
    artma / libs / formatting / results[capture_print_wide]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "t_stat", "study_id"))

  opt <- get_option_group("artma.methods.p_hacking_tests")

  resolved <- resolve_options(opt, list(
    include_caliper = opt_spec(default = TRUE, type = "logical"),
    caliper_thresholds = opt_spec(
      default = c(1.645, 1.96, 2.58), type = "numeric",
      constraint = function(x) length(x) > 0,
      constraint_msg = "caliper_thresholds must not be empty"
    ),
    caliper_widths = opt_spec(
      default = c(0.05, 0.1, 0.15), type = "numeric",
      constraint = function(x) length(x) > 0,
      constraint_msg = "caliper_widths must not be empty"
    ),
    caliper_display_ratios = opt_spec(default = TRUE, type = "logical"),
    caliper_tail = opt_spec(
      default = "auto", type = "character",
      constraint = function(x) x %in% c("auto", "positive", "negative", "absolute"),
      constraint_msg = "caliper_tail must be one of: auto, positive, negative, absolute"
    ),
    caliper_cluster = opt_spec(default = TRUE, type = "logical"),
    include_elliott = opt_spec(default = TRUE, type = "logical"),
    elliott_supports = opt_spec(
      default = c(0.05, 0.1), type = "numeric",
      constraint = function(x) {
        length(x) > 0 && all(is.finite(x) & x > 0 & x <= 1) &&
          !is.unsorted(x, strictly = TRUE)
      },
      constraint_msg = "elliott_supports must be strictly increasing p-value bounds in (0, 1]"
    ),
    lcm_iterations = opt_spec(
      default = 10000L, type = "numeric", cast = as.integer,
      constraint = function(x) x > 0, constraint_msg = "lcm_iterations must be positive"
    ),
    lcm_grid_points = opt_spec(
      default = 3000L, type = "numeric", cast = as.integer,
      constraint = function(x) x > 0, constraint_msg = "lcm_grid_points must be positive"
    ),
    simulate_cdfs_chunk_size = opt_spec(
      default = 512L, type = "numeric", from = "simulate_cdfs.chunk_size", cast = as.integer,
      constraint = function(x) x > 0, constraint_msg = "simulate_cdfs.chunk_size must be positive"
    ),
    simulate_cdfs_seed = opt_spec(
      default = 123L, type = "numeric", from = "simulate_cdfs.seed",
      allow_na = TRUE, cast = as.integer
    ),
    include_discontinuity = opt_spec(default = TRUE, type = "logical"),
    discontinuity_bandwidth = opt_spec(
      default = NA_real_, type = "numeric", allow_na = TRUE,
      constraint = function(x) x > 0, constraint_msg = "discontinuity_bandwidth must be positive"
    ),
    include_cox_shi = opt_spec(default = TRUE, type = "logical"),
    cox_shi_bins = opt_spec(
      default = 10L, type = "numeric", cast = as.integer,
      constraint = function(x) x > 0, constraint_msg = "cox_shi_bins must be positive"
    ),
    cox_shi_order = opt_spec(
      default = 2L, type = "numeric", cast = as.integer,
      constraint = function(x) x >= 0, constraint_msg = "cox_shi_order must be non-negative"
    ),
    cox_shi_bounds = opt_spec(
      default = 1L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1), constraint_msg = "cox_shi_bounds must be 0 or 1"
    ),
    round_to = opt_spec(
      default = 3L, type = "numeric", key = "artma.output.number_of_decimals", cast = as.integer,
      constraint = function(x) x >= 0, constraint_msg = "Number of decimals must be non-negative"
    )
  ))

  # An NA sentinel means "unset" for these two; convert to NULL, matching the
  # prior code that turned an unset seed/bandwidth into NULL before dispatch.
  simulate_cdfs_seed <- if (length(resolved$simulate_cdfs_seed) == 1 && is.na(resolved$simulate_cdfs_seed)) {
    NULL
  } else {
    resolved$simulate_cdfs_seed
  }
  discontinuity_bandwidth <- if (length(resolved$discontinuity_bandwidth) == 1 && is.na(resolved$discontinuity_bandwidth)) {
    NULL
  } else {
    resolved$discontinuity_bandwidth
  }

  resolved_options <- list(
    include_caliper = resolved$include_caliper,
    caliper_thresholds = resolved$caliper_thresholds,
    caliper_widths = resolved$caliper_widths,
    caliper_display_ratios = resolved$caliper_display_ratios,
    caliper_tail = resolved$caliper_tail,
    caliper_cluster = resolved$caliper_cluster,
    include_elliott = resolved$include_elliott,
    elliott_supports = resolved$elliott_supports,
    lcm_iterations = resolved$lcm_iterations,
    lcm_grid_points = resolved$lcm_grid_points,
    simulate_cdfs_chunk_size = resolved$simulate_cdfs_chunk_size,
    simulate_cdfs_seed = simulate_cdfs_seed,
    include_discontinuity = resolved$include_discontinuity,
    discontinuity_bandwidth = discontinuity_bandwidth,
    include_cox_shi = resolved$include_cox_shi,
    cox_shi_bins = resolved$cox_shi_bins,
    cox_shi_order = resolved$cox_shi_order,
    cox_shi_bounds = resolved$cox_shi_bounds,
    add_significance_marks = resolve_add_significance_marks(),
    round_to = resolved$round_to
  )

  results <- run_p_hacking_tests(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("P-hacking tests")

    # Caliper tests (Gerber & Malhotra, 2008)
    if (!is.null(results$caliper) && nrow(results$caliper) > 0) {
      cli::cli_h3("Caliper tests (Gerber & Malhotra, 2008)")
      cli::cli_text("Tests for discontinuities in t-statistic distributions around significance thresholds.")

      tail_used <- attr(results$caliper, "caliper_tail") %||% "positive"
      tail_note <- switch(tail_used,
        positive = "positive t-statistics (t > threshold)",
        negative = "negative t-statistics (t < -threshold)",
        absolute = "absolute t-statistics (|t| > threshold)",
        tail_used
      )
      cli::cli_text("Tail inspected: {tail_note}. Only an excess {.emph above} the threshold indicates p-hacking.")

      if (identical(attr(results$caliper, "caliper_cluster_method"), "study")) {
        cli::cli_text("P-values are clustered at the study level.")
      }

      caliper_lines <- capture_print_wide(results$caliper, row.names = FALSE)
      cli::cli_verbatim(caliper_lines)
      cli::cli_text("")
    }

    # Elliott tests (2022)
    if (!is.null(results$elliott) && nrow(results$elliott) > 0) {
      cli::cli_h3("Elliott et al. (2022) tests")
      cli::cli_text(
        "These test global monotonicity of the p-value density, so they can legitimately ",
        "disagree with the local caliper results above."
      )

      elliott_lines <- capture_print_wide(results$elliott, row.names = FALSE)
      cli::cli_verbatim(elliott_lines)

      # Footnote the NA cells: each skipped test carries a reason (e.g. a
      # singular Cox-Shi bin covariance) that would otherwise be lost.
      for (item in results$skipped) {
        cli::cli_alert_warning("Skipped {item$label}: {item$reason}")
      }
      cli::cli_text("")
    }

    # Overall note
    if (!is.null(results$caliper) || !is.null(results$elliott)) {
      cli::cli_text("Note: Low p-values indicate potential p-hacking or selective reporting.")
      cli::cli_text("Significance marks: * p <= 0.1, ** p <= 0.05, *** p <= 0.01")
    } else {
      cli::cli_alert_warning("No p-hacking tests were successfully completed.")
    }
  }

  invisible(new_method_result(
    tables = list(
      caliper = results$caliper,
      elliott = results$elliott
    ),
    estimates = p_hacking_tests_estimates(results),
    meta = list(skipped_models = results$skipped)
  ))
}

#' @title Tidy estimates for the p-hacking tests
#' @description
#' Flatten the caliper grid and the Elliott test battery into the shared
#' `estimates` schema. Neither display table is a coefficient table, so the
#' dimension mapping is a judgement call rather than a rename:
#'
#' * Caliper. The display table is a grid: one column per t-statistic
#'   threshold, four stacked rows per caliper width, with cells such as
#'   `"3/6 (6 studies)"`. In the long schema each threshold-width pair is one
#'   row, `model` is the threshold (`"threshold_1.96"`) and `term` is the
#'   caliper width (`"width_0.05"`). `estimate` is the share of the window
#'   above the threshold, `p_value` its test p-value, `n_obs` the number of
#'   estimates inside the window, and `n_clusters` the number of studies they
#'   come from. The above/below split is not a separate column: `n_above` is
#'   `estimate * n_obs`, and the direction (which side carries the excess) is
#'   in `note` together with the tail inspected.
#' * Elliott. Each test is one row, `model` is the test family (`"binomial"`,
#'   `"lcm"`, `"fisher"`, `"discontinuity"`, `"cox_shi"`) and `term` the
#'   p-value support it was run on (`"[0, 0.05]"`). These tests report a
#'   p-value and nothing else, so only `p_value` is filled; a test that could
#'   not run keeps its `NA` and carries the reason in `note`.
#'
#' The observation counts printed at the foot of the Elliott table are sample
#' descriptions rather than test results, and stay in the display table.
#' @param results *\[list\]* The value returned by `run_p_hacking_tests()`.
#' @return *\[data.frame\]* A frame in the shared estimates schema.
p_hacking_tests_estimates <- function(results) {
  box::use(
    artma / modules / runtime_methods[new_estimates]
  )

  caliper_rows <- function(caliper_results) {
    if (!is.list(caliper_results) || length(caliper_results) == 0L) {
      return(NULL)
    }

    do.call(rbind, lapply(caliper_results, function(res) {
      direction <- res$direction
      note <- paste0(
        "tail: ", res$tail %||% NA_character_,
        "; direction: ", if (is.null(direction) || is.na(direction)) "unknown" else direction
      )
      n_studies <- res$n_studies
      data.frame(
        method = "p_hacking_tests",
        model = paste0("threshold_", res$threshold),
        term = paste0("width_", res$width),
        estimate = as.numeric(res$share_above),
        p_value = as.numeric(res$p_value),
        n_obs = res$n_above + res$n_below,
        n_clusters = if (is.null(n_studies) || !is.finite(n_studies) || n_studies == 0L) {
          NA_integer_
        } else {
          as.integer(n_studies)
        },
        note = note,
        stringsAsFactors = FALSE
      )
    }))
  }

  elliott_rows <- function(elliott_results) {
    if (!is.list(elliott_results) || length(elliott_results) == 0L) {
      return(NULL)
    }

    do.call(rbind, lapply(elliott_results, function(test) {
      data.frame(
        method = "p_hacking_tests",
        model = test$family %||% "elliott",
        term = test$support %||% test$test,
        p_value = as.numeric(test$p_value),
        note = attr(test$p_value, "reason") %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }))
  }

  rows <- Filter(Negate(is.null), list(
    caliper_rows(results$caliper_results),
    elliott_rows(results$elliott_results)
  ))

  if (length(rows) == 0L) {
    return(new_estimates())
  }

  new_estimates(do.call(rbind, lapply(rows, new_estimates)))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  p_hacking_tests,
  stage = "p_hacking_tests",
  required_columns = c("effect", "se", "t_stat", "study_id")
)

box::export(p_hacking_tests, p_hacking_tests_estimates, run)
