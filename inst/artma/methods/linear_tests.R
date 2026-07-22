#' @title Linear model diagnostics
#' @description
#' Run a suite of linear-model based publication bias diagnostics, including
#' fixed, random, and weighted variants. The function returns both tidy
#' coefficient estimates and a publication-ready summary table.
linear_tests <- function(df) {
  box::use(
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / libs / core / utils[get_verbosity],
    artma / libs / formatting / results[print_summary_table],
    artma / econometric / linear[run_linear_models],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "study_id"))

  opt <- get_option_group("artma.methods.linear_tests")

  add_marks <- resolve_add_significance_marks()
  bootstrap_replications <- opt$bootstrap_replications %||% 999L
  conf_level <- opt$conf_level %||% 0.95
  round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))

  validate(
    is.logical(add_marks),
    is.numeric(bootstrap_replications),
    is.numeric(conf_level),
    is.numeric(round_to)
  )

  bootstrap_replications <- as.integer(bootstrap_replications)
  assert(bootstrap_replications >= 0, "Bootstrap replications must be greater than or equal to 0.")
  assert(conf_level > 0 && conf_level < 1, "Confidence level must lie in the (0, 1) interval.")
  assert(round_to >= 0, "Number of decimals must be non-negative.")

  resolved_options <- list(
    add_significance_marks = add_marks,
    bootstrap_replications = bootstrap_replications,
    conf_level = conf_level,
    round_to = round_to
  )

  verbosity <- get_verbosity()

  # Below this many expected draws per tail, the percentile CI bound is
  # effectively pinned by a handful of extreme resamples and swings with the
  # RNG seed rather than converging.
  min_tail_draws <- 10
  tail_draws <- bootstrap_replications * (1 - conf_level) / 2
  if (verbosity >= 1 && bootstrap_replications > 0 && tail_draws < min_tail_draws) {
    cli::cli_alert_warning(paste(
      "Only {bootstrap_replications} bootstrap replications requested",
      "(~{round(tail_draws, 1)} draws per confidence interval tail at the",
      "{round(conf_level * 100)}% level). Percentile bootstrap CI bounds can",
      "be unstable with so few draws, especially when {.field se} spans",
      "several orders of magnitude. Consider raising",
      "{.field artma.methods.linear_tests.bootstrap_replications}."
    ))
  }

  results <- run_linear_models(df, resolved_options)

  if (verbosity >= 1) {
    cli::cli_h2("Linear model tests")

    if (nrow(results$summary) > 0) {
      print_summary_table(results$summary)
    } else {
      cli::cli_alert_warning("No linear models were successfully estimated.")
    }

    n_conflicts <- sum(results$coefficients$ci_conflict, na.rm = TRUE)
    if (n_conflicts > 0) {
      cli::cli_alert_warning(paste(
        "{n_conflicts} out of {nrow(results$coefficients)} estimates are",
        "marked with †: the analytic significance test and the",
        "{round(conf_level * 100)}% bootstrap confidence interval disagree",
        "on whether the estimate differs from zero. Treat these estimates",
        "with caution."
      ))
    }

    if (length(results$skipped) > 0 && verbosity >= 2) {
      for (item in results$skipped) {
        cli::cli_alert_warning("{item$label}: {item$reason}")
      }
    }
  }

  invisible(new_method_result(
    tables = list(summary = results$summary),
    meta = list(
      coefficients = results$coefficients,
      skipped = results$skipped,
      options = results$options
    )
  ))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  linear_tests,
  stage = "linear_tests",
  required_columns = c("effect", "se", "study_id")
)

box::export(linear_tests, run)
