#' @title P-hacking tests
#' @description
#' Run a comprehensive suite of publication bias tests designed to detect
#' p-hacking and selective reporting. Tests include:
#' - Caliper tests (Gerber & Malhotra, 2008)
#' - Elliott et al. (2022) tests (Binomial, LCM, Fisher, Discontinuity, Cox-Shi)
#' - MAIVE estimator (Irsova et al., 2023)
p_hacking_tests <- function(df) {
  box::use(
    artma / libs / validation[assert, validate, validate_columns],
    artma / libs / utils[get_verbosity],
    artma / libs / p_hacking_tests[run_p_hacking_tests],
    artma / options / index[get_option_group],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "t_stat", "study_id"))

  opt <- get_option_group("artma.methods.p_hacking_tests")

  # Caliper options
  include_caliper <- opt$include_caliper %||% TRUE
  caliper_thresholds <- opt$caliper_thresholds %||% c(0, 1.96, 2.58)
  caliper_widths <- opt$caliper_widths %||% c(0.05, 0.1, 0.2)

  # Elliott options
  include_elliott <- opt$include_elliott %||% TRUE
  lcm_iterations <- opt$lcm_iterations %||% 10000L
  lcm_grid_points <- opt$lcm_grid_points %||% 10000L
  include_discontinuity <- opt$include_discontinuity %||% TRUE
  discontinuity_bandwidth <- opt$discontinuity_bandwidth %||% 0.05
  include_cox_shi <- opt$include_cox_shi %||% TRUE
  cox_shi_bins <- opt$cox_shi_bins %||% 20L
  cox_shi_order <- opt$cox_shi_order %||% 2L
  cox_shi_bounds <- opt$cox_shi_bounds %||% 1L

  # MAIVE options
  include_maive <- opt$include_maive %||% TRUE
  maive_method <- opt$maive_method %||% 3L
  maive_weight <- opt$maive_weight %||% 0L
  maive_instrument <- opt$maive_instrument %||% 1L
  maive_studylevel <- opt$maive_studylevel %||% 2L
  maive_se <- opt$maive_se %||% 3L
  maive_ar <- opt$maive_ar %||% 1L
  maive_first_stage <- opt$maive_first_stage %||% 0L

  # General options
  add_significance_marks <- resolve_add_significance_marks()
  round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))

  validate(
    is.logical(include_caliper),
    is.numeric(caliper_thresholds),
    is.numeric(caliper_widths),
    is.logical(include_elliott),
    is.numeric(lcm_iterations),
    is.numeric(lcm_grid_points),
    is.logical(include_discontinuity),
    is.numeric(discontinuity_bandwidth),
    is.logical(include_cox_shi),
    is.numeric(cox_shi_bins),
    is.numeric(cox_shi_order),
    is.numeric(cox_shi_bounds),
    is.logical(include_maive),
    is.numeric(maive_method),
    is.numeric(maive_weight),
    is.numeric(maive_instrument),
    is.numeric(maive_studylevel),
    is.numeric(maive_se),
    is.numeric(maive_ar),
    is.numeric(maive_first_stage),
    is.logical(add_significance_marks),
    is.numeric(round_to)
  )

  assert(length(caliper_thresholds) > 0, "caliper_thresholds must not be empty")
  assert(length(caliper_widths) > 0, "caliper_widths must not be empty")
  assert(lcm_iterations > 0, "lcm_iterations must be positive")
  assert(lcm_grid_points > 0, "lcm_grid_points must be positive")
  assert(discontinuity_bandwidth > 0, "discontinuity_bandwidth must be positive")
  assert(cox_shi_bins > 0, "cox_shi_bins must be positive")
  assert(cox_shi_order >= 0, "cox_shi_order must be non-negative")
  assert(cox_shi_bounds %in% c(0, 1), "cox_shi_bounds must be 0 or 1")
  assert(maive_method %in% c(1, 2, 3, 4), "maive_method must be 1, 2, 3, or 4")
  assert(maive_weight %in% c(0, 1, 2), "maive_weight must be 0, 1, or 2")
  assert(maive_instrument %in% c(0, 1), "maive_instrument must be 0 or 1")
  assert(maive_studylevel %in% c(0, 1, 2), "maive_studylevel must be 0, 1, or 2")
  assert(maive_se %in% c(1, 2, 3, 4, 5), "maive_se must be 1, 2, 3, 4, or 5")
  assert(maive_ar %in% c(0, 1), "maive_ar must be 0 or 1")
  assert(round_to >= 0, "Number of decimals must be non-negative")

  resolved_options <- list(
    include_caliper = include_caliper,
    caliper_thresholds = caliper_thresholds,
    caliper_widths = caliper_widths,
    include_elliott = include_elliott,
    lcm_iterations = as.integer(lcm_iterations),
    lcm_grid_points = as.integer(lcm_grid_points),
    include_discontinuity = include_discontinuity,
    discontinuity_bandwidth = discontinuity_bandwidth,
    include_cox_shi = include_cox_shi,
    cox_shi_bins = as.integer(cox_shi_bins),
    cox_shi_order = as.integer(cox_shi_order),
    cox_shi_bounds = as.integer(cox_shi_bounds),
    include_maive = include_maive,
    maive_method = as.integer(maive_method),
    maive_weight = as.integer(maive_weight),
    maive_instrument = as.integer(maive_instrument),
    maive_studylevel = as.integer(maive_studylevel),
    maive_se = as.integer(maive_se),
    maive_ar = as.integer(maive_ar),
    maive_first_stage = as.integer(maive_first_stage),
    add_significance_marks = add_significance_marks,
    round_to = round_to
  )

  results <- run_p_hacking_tests(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("P-hacking tests")

    # Caliper tests (Gerber & Malhotra, 2008)
    if (!is.null(results$caliper) && nrow(results$caliper) > 0) {
      cli::cli_h3("Caliper tests (Gerber & Malhotra, 2008)")
      cli::cli_text("Tests for discontinuities in t-statistic distributions around significance thresholds.")

      caliper_lines <- utils::capture.output(
        print(results$caliper, row.names = TRUE) # nolint: undesirable_function_linter.
      )
      cli::cli_verbatim(caliper_lines)
      cli::cli_text("")
    }

    # Elliott tests (2022)
    if (!is.null(results$elliott) && nrow(results$elliott) > 0) {
      cli::cli_h3("Elliott et al. (2022) tests")
      cli::cli_text("Comprehensive p-hacking detection tests including Binomial, LCM, Fisher, Discontinuity, and Cox-Shi.")

      elliott_lines <- utils::capture.output(
        print(results$elliott, row.names = TRUE) # nolint: undesirable_function_linter.
      )
      cli::cli_verbatim(elliott_lines)
      cli::cli_text("")
    }

    # MAIVE estimator (Irsova et al., 2023)
    if (!is.null(results$maive) && nrow(results$maive) > 0) {
      cli::cli_h3("MAIVE estimator (Irsova et al., 2023)")
      cli::cli_text("Meta-Analysis Instrumental Variable Estimator for publication bias correction.")

      maive_lines <- utils::capture.output(
        print(results$maive, row.names = FALSE) # nolint: undesirable_function_linter.
      )
      cli::cli_verbatim(maive_lines)
      cli::cli_text("")
    }

    # Overall note
    if (!is.null(results$caliper) || !is.null(results$elliott) || !is.null(results$maive)) {
      cli::cli_text("Note: Low p-values indicate potential p-hacking or selective reporting.")
      cli::cli_text("Significance marks: * p < 0.05, ** p < 0.01, *** p < 0.001")
    } else {
      cli::cli_alert_warning("No p-hacking tests were successfully completed.")
    }

    if (!is.null(results$skipped) && verbosity >= 2) {
      for (msg in results$skipped) {
        cli::cli_alert_warning("Skipped: {msg}")
      }
    }
  }

  results
}

box::use(
  artma / libs / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  p_hacking_tests,
  stage = "p_hacking_tests",
  key_builder = function(...) build_data_cache_signature()
)

box::export(p_hacking_tests, run)
