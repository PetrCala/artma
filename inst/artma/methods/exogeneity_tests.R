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
      iv_instrument = opt_spec(default = "1/sqrt(n_obs)", type = "character"),
      puniform_alpha = opt_spec(
        default = 0.05, type = "numeric",
        constraint = function(x) x > 0 && x < 1,
        constraint_msg = "puniform_alpha must lie in the (0, 1) interval."
      ),
      puniform_method = opt_spec(default = "ML", type = "character"),
      puniform_side = opt_spec(default = "auto", type = "character"),
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
    estimates = exogeneity_tests_estimates(results),
    meta = list(
      iv = results$iv,
      puniform = results$puniform,
      skip_reason = results$skipped
    )
  ))
}

#' @title Tidy estimates for the exogeneity tests
#' @description
#' Map the IV and p-uniform* coefficient frames onto the shared `estimates`
#' schema. Both estimators contribute their own `model` (`"iv"`,
#' `"puniform"`), and their `term` values carry over unchanged. The numbers are
#' taken verbatim: the formatted columns sitting alongside them belong to the
#' display table only.
#'
#' The two diagnostics that are not coefficients ride along as notes rather
#' than rows: a weak first stage on the IV rows, and the p-uniform*
#' convergence/fallback note on its rows.
#' @param results *\[list\]* The value returned by `run_exogeneity_tests()`.
#' @return *\[data.frame\]* A frame in the shared estimates schema.
exogeneity_tests_estimates <- function(results) {
  box::use(
    artma / modules / runtime_methods[new_estimates]
  )

  as_rows <- function(coefficients, model, note) {
    if (!is.data.frame(coefficients) || nrow(coefficients) == 0L) {
      return(NULL)
    }
    data.frame(
      method = "exogeneity_tests",
      model = model,
      term = coefficients$term,
      estimate = coefficients$estimate,
      std_error = coefficients$std_error,
      statistic = coefficients$statistic,
      p_value = coefficients$p_value,
      n_obs = coefficients$n_obs,
      note = note %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }

  iv <- results$iv
  iv_note <- if (isTRUE(iv$weak_instrument)) {
    "First-stage F below 10; the instrument is weak"
  } else {
    NA_character_
  }

  rows <- Filter(Negate(is.null), list(
    as_rows(iv$coefficients, "iv", iv_note),
    as_rows(results$puniform$coefficients, "puniform", results$puniform$note)
  ))

  if (length(rows) == 0L) {
    return(new_estimates())
  }

  new_estimates(do.call(rbind, rows))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  exogeneity_tests,
  stage = "exogeneity_tests",
  description = "Publication-bias tests that relax exogeneity (IV regression, p-uniform*)",
  required_columns = c("effect", "se", "study_id", "n_obs", "study_size"),
  suggests = "AER"
)

box::export(exogeneity_tests, exogeneity_tests_estimates, run)
