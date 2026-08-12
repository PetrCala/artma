#' @title MAIVE estimator
#' @description
#' Meta-Analysis Instrumental Variable Estimator (Irsova et al., Nature
#' Communications, 2025). MAIVE corrects the mean effect for publication bias,
#' p-hacking, and spurious precision by instrumenting reported variances with
#' the inverse sample size, then plugging the fitted variances into a funnel
#' model (PET, PEESE, PET-PEESE, or the endogenous kink).
maive_estimator <- function(df) {
  box::use(
    artma / libs / core / validation[validate, validate_columns],
    artma / libs / core / utils[get_verbosity],
    artma / libs / formatting / results[print_sectioned_table, print_paragraph],
    artma / econometric / maive[maive_first_stage_f, maive_first_stage_is_weak, run_maive],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / options / significance_marks[resolve_add_significance_marks]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se", "n_obs"))

  opt <- get_option_group("artma.methods.maive")

  resolved_options <- resolve_options(opt, list(
    method = opt_spec(
      default = 3L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(1, 2, 3, 4),
      constraint_msg = "maive method must be 1, 2, 3, or 4"
    ),
    weight = opt_spec(
      default = 0L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1, 2),
      constraint_msg = "maive weight must be 0, 1, or 2"
    ),
    instrument = opt_spec(
      default = 1L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1),
      constraint_msg = "maive instrument must be 0 or 1"
    ),
    studylevel = opt_spec(
      default = 2L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1, 2),
      constraint_msg = "maive studylevel must be 0, 1, or 2"
    ),
    se = opt_spec(
      default = 1L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1, 2, 3),
      constraint_msg = "maive se must be 0, 1, 2, or 3"
    ),
    ar = opt_spec(
      default = 0L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1),
      constraint_msg = "maive ar must be 0 or 1"
    ),
    first_stage = opt_spec(
      default = 0L, type = "numeric", cast = as.integer,
      constraint = function(x) x %in% c(0, 1, 2),
      constraint_msg = "maive first_stage must be 0, 1, or 2"
    ),
    seed = opt_spec(default = 123L, type = "numeric", cast = as.integer),
    round_to = opt_spec(
      default = 3L, type = "numeric", key = "artma.output.number_of_decimals",
      cast = as.integer,
      constraint = function(x) x >= 0,
      constraint_msg = "Number of decimals must be non-negative"
    )
  ))
  resolved_options$add_significance_marks <- resolve_add_significance_marks()

  # show_interpretation drives display only and is not part of resolved_options
  # (which is echoed into the method result meta), so it is resolved separately.
  show_interpretation <- resolve_options(opt, list(
    show_interpretation = opt_spec(default = TRUE, type = "logical")
  ))$show_interpretation

  method <- resolved_options$method
  instrument <- resolved_options$instrument
  add_significance_marks <- resolved_options$add_significance_marks

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
      skip_reason = result$skipped
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
