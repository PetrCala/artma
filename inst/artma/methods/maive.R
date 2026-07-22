#' @title MAIVE estimator
#' @description
#' Meta-Analysis Instrumental Variable Estimator (Irsova et al., Nature
#' Communications, 2025). MAIVE corrects the mean effect for publication bias,
#' p-hacking, and spurious precision by instrumenting reported variances with
#' the inverse sample size, then plugging the fitted variances into a funnel
#' model (PET, PEESE, PET-PEESE, or the endogenous kink).
maive_estimator <- function(df) {
  box::use(
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / libs / core / utils[get_verbosity],
    artma / libs / formatting / results[print_sectioned_table, print_paragraph],
    artma / econometric / maive[maive_first_stage_f, maive_first_stage_is_weak, run_maive],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "n_obs"))

  opt <- get_option_group("artma.methods.maive")

  method <- opt$method %||% 3L
  weight <- opt$weight %||% 0L
  instrument <- opt$instrument %||% 1L
  studylevel <- opt$studylevel %||% 2L
  se <- opt$se %||% 1L
  ar <- opt$ar %||% 0L
  first_stage <- opt$first_stage %||% 0L
  seed <- opt$seed %||% 123L
  show_interpretation <- opt$show_interpretation %||% TRUE

  add_significance_marks <- resolve_add_significance_marks()
  round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))

  validate(
    is.numeric(method),
    is.numeric(weight),
    is.numeric(instrument),
    is.numeric(studylevel),
    is.numeric(se),
    is.numeric(ar),
    is.numeric(first_stage),
    is.numeric(seed),
    is.logical(show_interpretation),
    is.logical(add_significance_marks),
    is.numeric(round_to)
  )

  assert(method %in% c(1, 2, 3, 4), "maive method must be 1, 2, 3, or 4")
  assert(weight %in% c(0, 1, 2), "maive weight must be 0, 1, or 2")
  assert(instrument %in% c(0, 1), "maive instrument must be 0 or 1")
  assert(studylevel %in% c(0, 1, 2), "maive studylevel must be 0, 1, or 2")
  assert(se %in% c(1, 2, 3, 4, 5), "maive se must be 1, 2, 3, 4, or 5")
  assert(ar %in% c(0, 1), "maive ar must be 0 or 1")
  assert(first_stage %in% c(0, 1, 2), "maive first_stage must be 0, 1, or 2")
  assert(round_to >= 0, "Number of decimals must be non-negative")

  resolved_options <- list(
    method = as.integer(method),
    weight = as.integer(weight),
    instrument = as.integer(instrument),
    studylevel = as.integer(studylevel),
    se = as.integer(se),
    ar = as.integer(ar),
    first_stage = as.integer(first_stage),
    seed = as.integer(seed),
    add_significance_marks = add_significance_marks,
    round_to = round_to
  )

  result <- run_maive(df, resolved_options)

  if (get_verbosity() >= 1) {
    cli::cli_h2("MAIVE estimator")

    if (is.null(result$summary)) {
      cli::cli_alert_warning("MAIVE was skipped: {result$skipped}")
    } else {
      print_sectioned_table(result$summary)

      # The weak-instrument note in the table is easy to skim past, and it is
      # the one diagnostic that invalidates the headline number, so it also
      # gets an alert one verbosity level below the interpretation paragraph.
      maive_f <- maive_first_stage_f(result$raw)
      if (get_verbosity() >= 2 && instrument == 1 && maive_first_stage_is_weak(maive_f)) {
        cli::cli_text("")
        cli::cli_alert_warning(c(
          "First-stage F is {round(maive_f, 3)}, below 10: the sample-size instrument ",
          "is weak and the MAIVE estimate is unreliable."
        ))
        if (!identical(result$first_stage$value, 1L)) {
          cli::cli_alert_info(
            "Try {.code first_stage: 1} (log first stage), which fits data whose sample sizes span orders of magnitude."
          )
        }
      }

      if (isTRUE(show_interpretation) && length(result$interpretation) > 0 && get_verbosity() >= 3) {
        cli::cli_text("")
        cli::cli_verbatim("Interpretation")
        print_paragraph(result$interpretation)
      }

      cli::cli_text("")
      if (add_significance_marks) {
        cli::cli_text("Significance marks: * p <= 0.1, ** p <= 0.05, *** p <= 0.01")
      }
      cli::cli_text("Method: Irsova, Bom, Havranek & Rachinger, Nature Communications, 2025.")
    }
  }

  invisible(new_method_result(
    tables = list(summary = result$summary),
    meta = list(
      maive = result$raw,
      interpretation = result$interpretation,
      first_stage = result$first_stage,
      options = resolved_options,
      skipped = result$skipped
    )
  ))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  maive_estimator,
  stage = "maive",
  required_columns = c("effect", "se", "n_obs"),
  suggests = "MAIVE"
)

box::export(maive_estimator, run)
