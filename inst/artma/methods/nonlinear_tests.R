#' @title Non-linear model diagnostics
#' @description Run publication bias diagnostics based on non-linear estimators.
nonlinear_tests <- function(df) {
  box::use(
    artma / libs / core / validation[validate, validate_columns, assert],
    artma / libs / core / utils[get_verbosity],
    artma / libs / formatting / results[print_summary_table],
    artma / econometric / nonlinear[run_nonlinear_methods],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "study_id"))

  opt <- get_option_group("artma.methods.nonlinear_tests")

  # round_to has a bespoke default: the method's own (NA) option falls back to
  # the global output decimals rather than a fixed literal, so it is resolved
  # before the spec-driven block.
  round_to_opt <- opt$round_to
  if (length(round_to_opt) == 1 && is.na(round_to_opt)) {
    round_to_opt <- NULL
  }
  round_to <- as.integer(round_to_opt %||% getOption("artma.output.number_of_decimals", 3))
  assert(round_to >= 0, "Number of decimals must be non-negative.")

  resolved_options <- c(
    list(add_significance_marks = resolve_add_significance_marks(), round_to = round_to),
    resolve_options(opt, list(
      stem_representative_sample = opt_spec(
        default = "medians", type = "character",
        constraint = function(x) x %in% c("medians", "first", "all"),
        constraint_msg = "Invalid STEM representative sample option."
      ),
      selection_cutoffs = opt_spec(default = c(1.96), type = "numeric"),
      selection_symmetric = opt_spec(default = FALSE, type = "logical"),
      selection_model = opt_spec(
        default = "normal", type = "character",
        constraint = function(x) x %in% c("normal", "t"),
        constraint_msg = "Selection model must be either 'normal' or 't'."
      ),
      hierarchical_iterations = opt_spec(
        default = 6000L, type = "numeric", cast = as.integer,
        constraint = function(x) x > 0,
        constraint_msg = "Hierarchical iterations must be positive."
      )
    ))
  )

  selection_cutoffs <- resolved_options$selection_cutoffs
  assert(length(selection_cutoffs) > 0, "Selection model requires at least one cutoff value.")
  assert(all(is.finite(selection_cutoffs)), "Selection cutoffs must be finite numbers.")
  assert(
    !is.unsorted(selection_cutoffs, strictly = TRUE),
    "Selection cutoffs must be strictly increasing, without duplicates."
  )

  results <- run_nonlinear_methods(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("Non-linear model tests")

    if (nrow(results$summary) > 0) {
      print_summary_table(results$summary)
    } else {
      cli::cli_alert_warning("No non-linear models were successfully estimated.")
    }

    if (length(results$skipped) > 0 && verbosity >= 2) {
      for (item in results$skipped) {
        cli::cli_alert_warning("{item$label}: {item$reason}")
      }
    }
  }

  stem_plots <- results$plots$stem %||% list()

  invisible(new_method_result(
    tables = list(summary = results$summary),
    plots = list(
      stem_funnel = stem_plots$stem_funnel,
      stem_mse = stem_plots$stem_mse
    ),
    meta = list(
      coefficients = results$coefficients,
      skipped_models = results$skipped,
      options = results$options
    )
  ))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  nonlinear_tests,
  stage = "nonlinear_tests",
  required_columns = c("effect", "se", "study_id")
)

box::export(nonlinear_tests, run)
