#' @title Linear model diagnostics
#' @description
#' Run a suite of linear-model based publication bias diagnostics, including
#' fixed, random, and weighted variants. The function returns both tidy
#' coefficient estimates and a publication-ready summary table.
linear_tests <- function(df) {
  box::use(
    artma / libs / validation[assert, validate, validate_columns],
    artma / libs / utils[get_verbosity],
    artma / libs / linear_tests[run_linear_models],
    artma / options / index[get_option_group],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "study_id"))

  opt <- get_option_group("artma.methods.linear_tests")

  add_marks <- resolve_add_significance_marks("artma.methods.linear_tests")
  bootstrap_replications <- opt$bootstrap_replications %||% 100L
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

  results <- run_linear_models(df, resolved_options)

  verbosity <- get_verbosity()

  if (verbosity >= 1) {
    cli::cli_h2("Linear model tests")

    if (nrow(results$summary) > 0) {
      summary <- results$summary

      duplicated_metric <- identical(rownames(summary), summary[[1]])
      if (duplicated_metric) {
        rownames(summary) <- NULL
      }

      # Hide synthetic row indices when the metric names already capture the labels.
      lines <- utils::capture.output(
        print(summary, row.names = !duplicated_metric) # nolint: undesirable_function_linter.
      )
      cli::cli_verbatim(lines) # print through cli for cache capture
    } else {
      cli::cli_alert_warning("No linear models were successfully estimated.")
    }

    if (length(results$skipped) > 0 && verbosity >= 2) {
      for (item in results$skipped) {
        cli::cli_alert_warning("{item$label}: {item$reason}")
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
  linear_tests,
  stage = "linear_tests",
  key_builder = function(...) build_data_cache_signature()
)

box::export(linear_tests, run)
