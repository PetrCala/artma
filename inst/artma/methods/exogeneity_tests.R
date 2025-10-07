#' @title Exogeneity tests
#' @description
#' Run publication bias diagnostics that relax the exogeneity assumption,
#' including instrumental variable (IV) regression and p-uniform* tests.
#' The function returns both detailed coefficients and a publication-ready summary table.
exogeneity_tests <- function(df) {
  box::use(
    artma / libs / validation[assert, validate, validate_columns],
    artma / libs / utils[get_verbosity],
    artma / libs / exogeneity_tests[run_exogeneity_tests],
    artma / options / index[get_option_group],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "study_id", "n_obs", "study_size"))

  opt <- get_option_group("artma.methods.exogeneity_tests")

  add_marks <- resolve_add_significance_marks("artma.methods.exogeneity_tests")
  iv_instrument <- opt$iv_instrument %||% "automatic"
  puniform_alpha <- opt$puniform_alpha %||% 0.05
  puniform_method <- opt$puniform_method %||% "ML"
  round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))

  validate(
    is.logical(add_marks),
    is.character(iv_instrument),
    is.numeric(puniform_alpha),
    is.character(puniform_method),
    is.numeric(round_to)
  )

  assert(puniform_alpha > 0 && puniform_alpha < 1, "puniform_alpha must lie in the (0, 1) interval.")
  assert(round_to >= 0, "Number of decimals must be non-negative.")

  resolved_options <- list(
    add_significance_marks = add_marks,
    iv_instrument = iv_instrument,
    puniform_alpha = puniform_alpha,
    puniform_method = puniform_method,
    round_to = round_to
  )

  results <- run_exogeneity_tests(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("Exogeneity tests")

    if (!is.null(results$iv$instrument_name)) {
      cli::cli_alert_info("Instrument used in IV regression: {.field {results$iv$instrument_name}}")
    }

    if (nrow(results$summary) > 0) {
      summary <- results$summary

      duplicated_metric <- identical(rownames(summary), summary[[1]])
      if (duplicated_metric) {
        rownames(summary) <- NULL
      }

      lines <- utils::capture.output(
        print(summary, row.names = !duplicated_metric) # nolint: undesirable_function_linter.
      )
      cli::cli_verbatim(lines)
    } else {
      cli::cli_alert_warning("No exogeneity tests were successfully estimated.")
    }

    if (!is.null(results$skipped) && verbosity >= 2) {
      cli::cli_alert_warning("Skipped: {results$skipped$reason}")
    }

    if (!is.null(results$iv$error) && verbosity >= 2) {
      cli::cli_alert_warning("IV regression error: {results$iv$error}")
    }

    if (!is.null(results$puniform$error) && verbosity >= 2) {
      cli::cli_alert_warning("p-uniform* error: {results$puniform$error}")
    }
  }

  results
}

box::use(
  artma / libs / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  exogeneity_tests,
  stage = "exogeneity_tests",
  key_builder = function(...) build_data_cache_signature()
)

box::export(exogeneity_tests, run)
