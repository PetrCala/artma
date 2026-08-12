#' @title Exogeneity tests
#' @description
#' Run publication bias diagnostics that relax the exogeneity assumption,
#' including instrumental variable (IV) regression and p-uniform* tests.
#' The function returns both detailed coefficients and a publication-ready summary table.
exogeneity_tests <- function(df) {
  box::use(
    artma / libs / core / validation[validate, validate_columns],
    artma / libs / core / utils[get_verbosity],
    artma / libs / formatting / results[print_summary_table],
    artma / econometric / exogeneity[run_exogeneity_tests],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "study_id", "n_obs", "study_size"))

  opt <- get_option_group("artma.methods.exogeneity_tests")

  resolved_options <- c(
    list(add_significance_marks = resolve_add_significance_marks()),
    resolve_options(opt, list(
      iv_instrument = opt_spec(default = "automatic", type = "character"),
      puniform_alpha = opt_spec(
        default = 0.05, type = "numeric",
        constraint = function(x) x > 0 && x < 1,
        constraint_msg = "puniform_alpha must lie in the (0, 1) interval."
      ),
      puniform_method = opt_spec(default = "ML", type = "character"),
      round_to = opt_spec(
        default = 3L, type = "numeric", key = "artma.output.number_of_decimals",
        cast = as.integer,
        constraint = function(x) x >= 0,
        constraint_msg = "Number of decimals must be non-negative."
      )
    ))
  )

  results <- run_exogeneity_tests(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("Exogeneity tests")

    if (!is.null(results$iv$instrument_name)) {
      cli::cli_alert_info("Instrument used in IV regression: {.field {results$iv$instrument_name}}")
    }

    if (nrow(results$summary) > 0) {
      print_summary_table(results$summary)
    } else {
      cli::cli_alert_warning("No exogeneity tests were successfully estimated.")
    }

    if (!is.null(results$skipped) && verbosity >= 2) {
      cli::cli_alert_warning("Skipped: {results$skipped}")
    }

    if (!is.null(results$iv$error) && verbosity >= 2) {
      cli::cli_alert_warning("IV regression error: {results$iv$error}")
    }

    if (!is.null(results$puniform$error) && verbosity >= 2) {
      cli::cli_alert_warning("p-uniform* error: {results$puniform$error}")
    }

    if (!is.null(results$puniform$note) && verbosity >= 2) {
      cli::cli_alert_warning("p-uniform* note: {results$puniform$note}")
    }
  }

  invisible(new_method_result(
    tables = list(summary = results$summary),
    meta = list(
      iv = results$iv,
      puniform = results$puniform,
      skip_reason = results$skipped
    )
  ))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  exogeneity_tests,
  stage = "exogeneity_tests",
  required_columns = c("effect", "se", "study_id", "n_obs", "study_size"),
  suggests = "AER"
)

box::export(exogeneity_tests, run)
