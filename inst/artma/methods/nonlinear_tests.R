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
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "study_id"))

  opt <- get_option_group("artma.methods.nonlinear_tests")

  add_marks <- resolve_add_significance_marks()
  round_to_opt <- opt$round_to
  if (length(round_to_opt) == 1 && is.na(round_to_opt)) {
    round_to_opt <- NULL
  }
  round_to <- round_to_opt %||% as.integer(getOption("artma.output.number_of_decimals", 3))
  stem_sample <- opt$stem_representative_sample %||% "medians"
  selection_cutoffs <- opt$selection_cutoffs %||% c(1.96)
  selection_symmetric <- opt$selection_symmetric %||% FALSE
  selection_model <- opt$selection_model %||% "normal"
  hierarchical_iterations <- opt$hierarchical_iterations %||% 6000L

  validate(
    is.logical(add_marks),
    is.numeric(round_to),
    is.character(stem_sample),
    is.numeric(selection_cutoffs),
    is.logical(selection_symmetric),
    is.character(selection_model),
    is.numeric(hierarchical_iterations)
  )

  round_to <- as.integer(round_to)
  hierarchical_iterations <- as.integer(hierarchical_iterations)

  assert(round_to >= 0, "Number of decimals must be non-negative.")
  assert(length(selection_cutoffs) > 0, "Selection model requires at least one cutoff value.")
  assert(all(selection_cutoffs > 0), "Selection cutoffs must be positive.")
  assert(selection_model %in% c("normal", "t"), "Selection model must be either 'normal' or 't'.")
  assert(hierarchical_iterations > 0, "Hierarchical iterations must be positive.")
  assert(stem_sample %in% c("medians", "first", "all"), "Invalid STEM representative sample option.")

  resolved_options <- list(
    add_significance_marks = add_marks,
    round_to = round_to,
    stem_representative_sample = stem_sample,
    selection_cutoffs = selection_cutoffs,
    selection_symmetric = selection_symmetric,
    selection_model = selection_model,
    hierarchical_iterations = hierarchical_iterations
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
  nonlinear_tests,
  stage = "nonlinear_tests",
  required_columns = c("effect", "se", "study_id")
)

box::export(nonlinear_tests, run)
