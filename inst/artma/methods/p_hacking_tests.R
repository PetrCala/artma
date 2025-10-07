#' @title P-hacking tests
#' @description
#' Run a comprehensive suite of publication bias tests designed to detect
#' p-hacking and selective reporting. Based on Elliott, Kudrin & Wüthrich (2022).
#' Tests include Binomial, LCM, Fisher, Discontinuity, and Cox-Shi.
p_hacking_tests <- function(df) {
  box::use(
    artma / libs / validation[assert, validate, validate_columns],
    artma / libs / utils[get_verbosity],
    artma / libs / p_hacking_tests[run_p_hacking_tests],
    artma / options / index[get_option_group],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se"))

  opt <- get_option_group("artma.methods.p_hacking_tests")

  lcm_iterations <- opt$lcm_iterations %||% 10000L
  lcm_grid_points <- opt$lcm_grid_points %||% 10000L
  include_discontinuity <- opt$include_discontinuity %||% TRUE
  discontinuity_bandwidth <- opt$discontinuity_bandwidth %||% 0.05
  include_cox_shi <- opt$include_cox_shi %||% TRUE
  cox_shi_bins <- opt$cox_shi_bins %||% 20L
  cox_shi_order <- opt$cox_shi_order %||% 2L
  cox_shi_bounds <- opt$cox_shi_bounds %||% 1L
  round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))

  validate(
    is.numeric(lcm_iterations),
    is.numeric(lcm_grid_points),
    is.logical(include_discontinuity),
    is.numeric(discontinuity_bandwidth),
    is.logical(include_cox_shi),
    is.numeric(cox_shi_bins),
    is.numeric(cox_shi_order),
    is.numeric(cox_shi_bounds),
    is.numeric(round_to)
  )

  assert(lcm_iterations > 0, "lcm_iterations must be positive")
  assert(lcm_grid_points > 0, "lcm_grid_points must be positive")
  assert(discontinuity_bandwidth > 0, "discontinuity_bandwidth must be positive")
  assert(cox_shi_bins > 0, "cox_shi_bins must be positive")
  assert(cox_shi_order >= 0, "cox_shi_order must be non-negative")
  assert(cox_shi_bounds %in% c(0, 1), "cox_shi_bounds must be 0 or 1")
  assert(round_to >= 0, "Number of decimals must be non-negative")

  resolved_options <- list(
    lcm_iterations = as.integer(lcm_iterations),
    lcm_grid_points = as.integer(lcm_grid_points),
    include_discontinuity = include_discontinuity,
    discontinuity_bandwidth = discontinuity_bandwidth,
    include_cox_shi = include_cox_shi,
    cox_shi_bins = as.integer(cox_shi_bins),
    cox_shi_order = as.integer(cox_shi_order),
    cox_shi_bounds = as.integer(cox_shi_bounds),
    round_to = round_to
  )

  results <- run_p_hacking_tests(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("P-hacking tests")

    cli::cli_alert_info("Total p-values: {results$n_pvalues}")
    cli::cli_alert_info("Significant at 0.05: {results$n_significant_005} ({round(100 * results$n_significant_005 / results$n_pvalues, 1)}%)")
    cli::cli_alert_info("Significant at 0.10: {results$n_significant_010} ({round(100 * results$n_significant_010 / results$n_pvalues, 1)}%)")

    if (nrow(results$summary) > 0) {
      summary <- results$summary

      lines <- utils::capture.output(
        print(summary, row.names = FALSE) # nolint: undesirable_function_linter.
      )
      cli::cli_verbatim(lines)

      cli::cli_text("")
      cli::cli_text("Note: Tests assess evidence against the null hypothesis of no p-hacking.")
      cli::cli_text("Low p-values indicate potential p-hacking or selective reporting.")
    } else {
      cli::cli_alert_warning("No p-hacking tests were successfully completed.")
    }

    if (!is.null(results$skipped) && verbosity >= 2) {
      cli::cli_alert_warning("Skipped: {results$skipped$reason}")
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
