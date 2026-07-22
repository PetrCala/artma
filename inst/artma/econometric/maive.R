#' @title MAIVE estimator helpers
#' @description
#' Data preparation, verdict derivation, summary-table construction, and
#' plain-language interpretation for the MAIVE estimator (Irsova et al., 2025).
#' The estimator itself lives in `artma/calc/methods/maive`; everything here is
#' about turning its return value into something a reader can act on.
NULL

box::use(
  artma / libs / core / validation[validate],
  artma / libs / formatting / results[
    format_number,
    format_se,
    format_ci,
    significance_mark
  ],
  artma / calc / methods / maive[maive]
)

#' @title Threshold below which the first-stage instrument counts as weak
WEAK_INSTRUMENT_F <- 10

#' @title Threshold below which running the first stage in logs is advised
LOG_FIRST_STAGE_F <- 30

#' @title Anderson-Rubin interval width beyond which identification is dubious
WIDE_AR_CI <- 100

# first_stage = 2 asks artma to pick the functional form. The levels first
# stage regresses SE^2 on 1/N; once sample sizes span this many orders of
# magnitude, 1/N is numerically indistinguishable from zero for most of the
# sample and the regression is identified off the few smallest studies alone.
AUTO_FIRST_STAGE <- 2L
AUTO_ORDERS_THRESHOLD <- 3

# Option labels -------------------------------------------------------------

#' @title Human-readable label for the MAIVE funnel model
#' @param x *[integer]* The `method` option (1..4).
#' @return *[character]* Label for display.
maive_method_label <- function(x) {
  switch(as.character(x),
    "1" = "FAT-PET",
    "2" = "PEESE",
    "3" = "PET-PEESE",
    "4" = "Endogenous kink (EK)",
    as.character(x)
  )
}

#' @title Human-readable label for the MAIVE weighting scheme
#' @param x *[integer]* The `weight` option (0..2).
#' @return *[character]* Label for display.
maive_weight_label <- function(x) {
  switch(as.character(x),
    "0" = "none",
    "1" = "standard",
    "2" = "adjusted",
    as.character(x)
  )
}

#' @title Human-readable label for the MAIVE instrumenting switch
#' @param x *[integer]* The `instrument` option (0 or 1).
#' @return *[character]* Label for display.
maive_instrument_label <- function(x) {
  switch(as.character(x),
    "0" = "off (no IV correction)",
    "1" = "on (1/N instrument)",
    as.character(x)
  )
}

#' @title Human-readable label for the MAIVE study-level structure
#' @param x *[integer]* The `studylevel` option (0..2).
#' @return *[character]* Label for display.
maive_studylevel_label <- function(x) {
  switch(as.character(x),
    "0" = "none",
    "1" = "study fixed effects",
    "2" = "cluster-robust",
    as.character(x)
  )
}

#' @title Human-readable label for the MAIVE standard-error mode
#' @param x *[integer]* The `se` option (1..5).
#' @return *[character]* Label for display.
maive_se_label <- function(x) {
  switch(as.character(x),
    "1" = "asymptotic",
    "2" = "pairs cluster bootstrap",
    "3" = "wild bootstrap",
    "4" = "wild cluster bootstrap",
    "5" = "pairs bootstrap",
    as.character(x)
  )
}

#' @title First-stage specification and matching F-statistic label
#' @description
#' The first stage runs either in levels or in logs, and the F statistic tests a
#' different coefficient in each case. Showing which one produced the number is
#' the difference between a diagnostic and a bare figure.
#' @param x *[integer]* The `first_stage` option (0 = levels, 1 = logs).
#' @return *[list]* With `spec` (the regression written out) and `f_label`.
maive_first_stage_spec <- function(x) {
  if (identical(as.integer(x), 1L)) {
    return(list(
      spec = "log(SE^2) ~ log N (Duan smearing)",
      f_label = "First-stage F (gamma1)",
      f_phrase = "first-stage F on gamma1"
    ))
  }
  list(spec = "SE^2 ~ 1/N", f_label = "First-stage F", f_phrase = "first-stage F")
}

#' @title Choose the MAIVE first-stage functional form
#' @description
#' Resolves the `first_stage` option. Values 0 (levels) and 1 (log) pass
#' straight through; 2 asks artma to decide.
#'
#' The decision reads the sample-size column and nothing else. It never looks
#' at the effects, the standard errors, or any fitted statistic, so it cannot
#' become a search over specifications for a preferred coefficient: `N` is the
#' instrument and is taken as exogenous, which makes its own spread a
#' legitimate basis for a functional-form choice, in the same way a skewed
#' regressor justifies a log scale. Selecting instead on the first-stage F
#' would be a different thing entirely, since MAIVE's premise is that the
#' standard error is endogenous with respect to the effect.
#' @param first_stage *[numeric]* The configured value: 0, 1, or 2.
#' @param n_obs *[numeric]* Per-observation sample sizes (MAIVE's `Ns`).
#' @return *[list]* `value` (0 or 1), `automatic` (was it resolved here), and
#'   `orders` (orders of magnitude spanned by `n_obs`, `NA` when undecidable).
resolve_maive_first_stage <- function(first_stage, n_obs) {
  passthrough <- function(value) {
    list(value = as.integer(value), automatic = FALSE, orders = NA_real_)
  }

  if (!isTRUE(as.integer(first_stage) == AUTO_FIRST_STAGE)) {
    return(passthrough(first_stage))
  }

  usable <- n_obs[is.finite(n_obs) & n_obs > 0]
  if (length(usable) < 2L) {
    # Nothing to measure a spread over: keep MAIVE's own default.
    return(list(value = 0L, automatic = TRUE, orders = NA_real_))
  }

  orders <- log10(max(usable) / min(usable))
  list(
    value = if (orders >= AUTO_ORDERS_THRESHOLD) 1L else 0L,
    automatic = TRUE,
    orders = orders
  )
}

# Verdicts ------------------------------------------------------------------

#' @title Extract the first-stage F statistic from MAIVE output
#' @description MAIVE reports the statistic as the literal string "NA" when
#'   instrumenting is switched off, so a plain `as.numeric()` is not enough.
#' @param maive_output *[list]* Output from `maive()`.
#' @return *[numeric]* The F statistic, or `NA_real_` when unavailable.
maive_first_stage_f <- function(maive_output) {
  raw <- maive_output$`F-test`
  if (is.null(raw) || length(raw) != 1L || identical(raw, "NA")) {
    return(NA_real_)
  }
  val <- suppressWarnings(as.numeric(raw))
  if (!is.finite(val)) NA_real_ else val
}

#' @title Classify first-stage instrument strength
#' @param f_stat *[numeric]* The first-stage F statistic.
#' @return *[character]* One of `"strong"`, `"weak"`, or `"unknown"`.
instrument_strength <- function(f_stat) {
  if (!is.numeric(f_stat) || length(f_stat) != 1L || !is.finite(f_stat)) {
    return("unknown")
  }
  if (f_stat >= WEAK_INSTRUMENT_F) "strong" else "weak"
}

#' @title Flag a weak MAIVE first stage
#' @param f_stat *[numeric]* First-stage F statistic.
#' @return *[logical]* TRUE when the instrument is weak by the Stock-Yogo rule
#'   of thumb.
maive_first_stage_is_weak <- function(f_stat) {
  identical(instrument_strength(f_stat), "weak")
}

#' @title Does the Hausman statistic exceed its critical value?
#' @param statistic *[numeric]* The Hausman statistic.
#' @param critical *[numeric]* The 5% chi-squared critical value.
#' @return *[logical]* `TRUE`, `FALSE`, or `NA` when the test is uninformative.
hausman_rejects <- function(statistic, critical) {
  finite <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!finite(statistic) || !finite(critical)) {
    return(NA)
  }
  statistic > critical
}

#' @title Does a confidence interval exclude zero?
#' @param ci *[numeric]* A length-2 interval.
#' @return *[logical]* `TRUE`, `FALSE`, or `NA` when the interval is unusable.
ci_excludes_zero <- function(ci) {
  if (!is.numeric(ci) || length(ci) != 2L || !all(is.finite(ci))) {
    return(NA)
  }
  ci[[1L]] > 0 || ci[[2L]] < 0
}

#' @title Verdict text for an Anderson-Rubin interval
#' @param ci *[numeric]* The interval, or anything non-finite when unavailable.
#' @param computed *[logical]* Whether AR computation was requested.
#' @return *[list]* With `value`, `note`, and `tone`.
ar_ci_verdict <- function(ci, computed) {
  if (!isTRUE(computed)) {
    return(list(value = "not computed", note = "enable the ar option to compute it", tone = ""))
  }
  if (!is.numeric(ci) || length(ci) != 2L || !all(is.finite(ci))) {
    return(list(value = "NA", note = "no valid acceptance region found", tone = "bad"))
  }
  width <- abs(ci[[2L]] - ci[[1L]])
  if (width > WIDE_AR_CI) {
    return(list(value = NA_character_, note = "very wide: weak identification", tone = "bad"))
  }
  list(value = NA_character_, note = "weak-instrument robust", tone = "")
}

# Data preparation ----------------------------------------------------------

#' @title Prepare data for MAIVE
#' @param df *[data.frame]* Input data with effect, se, n_obs, study_id.
#' @return *[data.frame]* Data formatted for MAIVE (bs, sebs, Ns, studyid).
prepare_maive_data <- function(df) {
  validate(is.data.frame(df))

  required_cols <- c("effect", "se", "n_obs")
  validate(all(required_cols %in% colnames(df)))

  maive_data <- data.frame(
    bs = df$effect,
    sebs = df$se,
    Ns = df$n_obs,
    stringsAsFactors = FALSE
  )

  if ("study_id" %in% colnames(df)) {
    maive_data$studyid <- df$study_id
  }

  maive_data
}

#' @title Compute two-sided p-value from coefficient and SE
#' @param beta *[numeric]* Coefficient.
#' @param se *[numeric]* Standard error.
#' @return *[numeric]* P-value, or NA if inputs are not finite.
maive_p_from_coef <- function(beta, se) {
  if (!is.numeric(beta) || !is.numeric(se) || !is.finite(beta) || !is.finite(se) || se <= 0) {
    return(NA_real_)
  }
  2 * stats::pnorm(-abs(beta / se))
}

#' @title Normal-approximation confidence interval
#' @param beta *[numeric]* Coefficient.
#' @param se *[numeric]* Standard error.
#' @param level *[numeric]* Confidence level.
#' @return *[numeric]* Length-2 interval, or `c(NA, NA)` for degenerate inputs.
maive_ci <- function(beta, se, level = 0.95) {
  if (!is.numeric(beta) || !is.numeric(se) || !is.finite(beta) || !is.finite(se) || se <= 0) {
    return(c(NA_real_, NA_real_))
  }
  half_width <- stats::qnorm(1 - (1 - level) / 2) * se
  c(beta - half_width, beta + half_width)
}

# Summary table -------------------------------------------------------------

#' @title Build the sectioned MAIVE summary table
#' @description
#' Produces a tidy `Section` / `Statistic` / `Value` / `Note` frame. Sections are
#' data rather than blank separator rows, so the exported CSV keeps the grouping
#' and the console renderer can print real headings. The verdict colours ride
#' along in a `tone` attribute so they never reach the CSV.
#' @param maive_output *[list]* Output of [maive()].
#' @param options *[list]* Resolved MAIVE options.
#' @param data_info *[list, optional]* With `n_obs` and `n_studies`.
#' @param resolved_first_stage *[list, optional]* The first stage actually used,
#'   as returned by [resolve_maive_first_stage()]. Defaults to the configured
#'   value, which is only wrong when automatic selection was in play.
#' @return *[data.frame]* The summary table, with a `tone` attribute.
build_maive_summary <- function(maive_output, options, data_info = list(),
                                resolved_first_stage = NULL) {
  validate(is.list(maive_output), is.list(options))

  rd <- options$round_to %||% 3L
  marks <- !isFALSE(options$add_significance_marks)
  instrumented <- identical(as.integer(options$instrument %||% 1L), 1L)
  fixed_intercept <- identical(as.integer(options$studylevel %||% 2L), 1L)
  resolved_first_stage <- resolved_first_stage %||%
    resolve_maive_first_stage(options$first_stage %||% 0L, numeric(0))
  first_stage <- maive_first_stage_spec(resolved_first_stage$value)

  rows <- list()
  self_env <- environment()
  add <- function(section, stat, value, note = "", tone = "") {
    if (length(value) != 1L || is.na(value)) {
      value <- "NA"
    }
    self_env$rows[[length(self_env$rows) + 1L]] <- list(
      section = section, stat = stat, value = as.character(value),
      note = note, tone = tone
    )
  }
  mark <- function(p_value) if (marks) significance_mark(p_value) else ""

  # --- Specification ---
  spec <- "Specification"
  add(spec, "Model", maive_method_label(options$method %||% 3L))
  add(spec, "Instrument", maive_instrument_label(options$instrument %||% 1L))
  add(spec, "Weights", maive_weight_label(options$weight %||% 0L))
  add(spec, "Study-level structure", maive_studylevel_label(options$studylevel %||% 2L))
  add(spec, "Standard errors", maive_se_label(options$se %||% 1L))
  if (instrumented) {
    add(
      spec, "First-stage specification", first_stage$spec,
      note = if (isTRUE(resolved_first_stage$automatic)) {
        if (is.na(resolved_first_stage$orders)) {
          "chosen automatically (no usable spread in N)"
        } else {
          sprintf(
            "chosen automatically (N spans %s orders of magnitude)",
            format_number(resolved_first_stage$orders, 1L)
          )
        }
      } else {
        ""
      }
    )
  }
  if (is.numeric(data_info$n_obs)) {
    add(spec, "Observations", format(as.integer(data_info$n_obs)))
  }
  if (is.numeric(data_info$n_studies)) {
    add(spec, "Studies", format(as.integer(data_info$n_studies)))
  }

  # --- Corrected mean estimate ---
  est <- "Corrected mean estimate"
  beta <- maive_output$beta
  beta_se <- maive_output$SE
  beta_p <- maive_p_from_coef(beta, beta_se)
  beta_significant <- is.finite(beta_p) && beta_p <= 0.05
  add(
    est, "MAIVE estimate", paste0(format_number(beta, rd), mark(beta_p)),
    note = if (is.na(beta_p)) {
      ""
    } else if (beta_significant) {
      "significant at 5%"
    } else {
      "not significant at 5%"
    },
    tone = if (is.na(beta_p)) "" else if (beta_significant) "good" else "bad"
  )
  add(est, "Std. error", format_se(beta_se, rd))
  beta_ci <- maive_ci(beta, beta_se)
  add(est, "95% CI", format_ci(beta_ci[[1L]], beta_ci[[2L]], rd))

  ar <- ar_ci_verdict(maive_output$AR_CI, identical(as.integer(options$ar %||% 0L), 1L))
  ar_value <- ar$value
  if (is.na(ar_value)) {
    ar_value <- format_ci(maive_output$AR_CI[[1L]], maive_output$AR_CI[[2L]], rd)
  }
  add(est, "Anderson-Rubin 95% CI", ar_value, note = ar$note, tone = ar$tone)

  selected <- maive_output$petpeese_selected
  if (!is.null(selected) && !is.na(selected)) {
    add(est, "Model selected", selected, note = "chosen by the PET-PEESE rule")
  }

  beta_standard <- maive_output$beta_standard
  if (is.numeric(beta_standard) && is.finite(beta_standard)) {
    add(
      est, "Unadjusted estimate", format_number(beta_standard, rd),
      note = "same model without the IV correction"
    )
    if (is.numeric(maive_output$SE_standard) && is.finite(maive_output$SE_standard)) {
      add(est, "Std. error", format_se(maive_output$SE_standard, rd))
    }
  }

  # --- Publication bias and p-hacking ---
  bias <- "Publication bias and p-hacking"
  pub_p <- maive_output$`pub bias p-value`
  pub_p <- if (is.numeric(pub_p) && is.finite(pub_p)) pub_p else NA_real_
  bias_detected <- is.finite(pub_p) && pub_p <= 0.05
  add(
    bias, "Egger coefficient", paste0(format_number(maive_output$egger_coef, rd), mark(pub_p)),
    note = if (is.na(pub_p)) {
      ""
    } else if (bias_detected) {
      "bias detected at 5%"
    } else {
      "no bias detected at 5%"
    },
    tone = if (is.na(pub_p)) "" else if (bias_detected) "bad" else "good"
  )
  add(bias, "Std. error", format_se(maive_output$egger_se, rd))
  add(bias, "p-value", format_number(pub_p, rd))

  boot_ci <- maive_output$egger_boot_ci
  if (is.numeric(boot_ci) && length(boot_ci) == 2L && all(is.finite(boot_ci))) {
    excludes <- ci_excludes_zero(boot_ci)
    add(
      bias, "95% CI (bootstrap)", format_ci(boot_ci[[1L]], boot_ci[[2L]], rd),
      note = if (isTRUE(excludes)) "excludes 0" else "includes 0",
      tone = if (isTRUE(excludes)) "bad" else "good"
    )
  }

  egger_ar <- maive_output$egger_ar_ci
  if (is.numeric(egger_ar) && length(egger_ar) == 2L && all(is.finite(egger_ar))) {
    add(
      bias, "Anderson-Rubin 95% CI", format_ci(egger_ar[[1L]], egger_ar[[2L]], rd),
      note = "weak-instrument robust"
    )
  }

  peese_coef <- maive_output$peese_se2_coef
  if (is.numeric(peese_coef) && is.finite(peese_coef)) {
    add(bias, "Coefficient on SE^2", format_number(peese_coef, rd))
    peese_se <- maive_output$peese_se2_se
    if (is.numeric(peese_se) && is.finite(peese_se)) {
      add(bias, "Std. error", format_se(peese_se, rd))
    }
  }

  # --- Diagnostics ---
  if (instrumented) {
    diag <- "Diagnostics"
    f_stat <- maive_first_stage_f(maive_output)
    strength <- instrument_strength(f_stat)
    add(
      diag, first_stage$f_label, format_number(f_stat, rd),
      note = switch(strength,
        strong = sprintf("strong instrument (>= %s)", WEAK_INSTRUMENT_F),
        weak = sprintf("weak instrument (< %s)", WEAK_INSTRUMENT_F),
        ""
      ),
      tone = switch(strength,
        strong = "good",
        weak = "bad",
        ""
      )
    )

    if (!fixed_intercept) {
      rejects <- hausman_rejects(maive_output$Hausman, maive_output$Chi2)
      add(
        diag, "Hausman statistic", format_number(maive_output$Hausman, rd),
        note = if (is.na(rejects)) {
          "uninformative: IV and OLS variances too close"
        } else if (rejects) {
          "rejects OLS = IV at 5%"
        } else {
          "does not reject OLS = IV at 5%"
        },
        tone = if (is.na(rejects)) "" else if (rejects) "good" else "bad"
      )
      add(diag, "Chi-sq. critical value (5%)", format_number(maive_output$Chi2, rd))
    }
  }

  summary <- data.frame(
    Section = vapply(rows, `[[`, "", "section"),
    Statistic = vapply(rows, `[[`, "", "stat"),
    Value = vapply(rows, `[[`, "", "value"),
    Note = vapply(rows, `[[`, "", "note"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(summary, "tone") <- vapply(rows, `[[`, "", "tone")
  summary
}

# Interpretation ------------------------------------------------------------

#' @title Generate a plain-language reading of the MAIVE results
#' @description
#' Turns the numbers into the sentences a reader would otherwise have to derive:
#' whether the corrected effect differs from zero, whether the funnel is
#' asymmetric, and whether the instrument is strong enough to trust the point
#' estimate over the Anderson-Rubin interval.
#' @param maive_output *[list]* Output of [maive()].
#' @param options *[list]* Resolved MAIVE options.
#' @param resolved_first_stage *[list, optional]* The first stage actually used,
#'   as returned by [resolve_maive_first_stage()].
#' @return *[character]* Sentences, in reading order.
build_maive_interpretation <- function(maive_output, options, resolved_first_stage = NULL) {
  validate(is.list(maive_output), is.list(options))

  resolved_first_stage <- resolved_first_stage %||%
    resolve_maive_first_stage(options$first_stage %||% 0L, numeric(0))

  rd <- min(options$round_to %||% 3L, 2L)
  sentences <- character()
  self_env <- environment()
  say <- function(...) {
    self_env$sentences <- c(self_env$sentences, paste0(...))
  }

  # --- Effect ---
  beta <- maive_output$beta
  beta_se <- maive_output$SE
  beta_ci <- maive_ci(beta, beta_se)
  beta_p <- maive_p_from_coef(beta, beta_se)
  model_label <- maive_method_label(options$method %||% 3L)

  if (is.finite(beta)) {
    verdict <- if (is.na(beta_p)) {
      "of undetermined significance"
    } else if (beta_p <= 0.05) {
      "statistically different from zero at the 5% level"
    } else {
      "not different from zero at the 5% level"
    }
    ci_text <- if (all(is.finite(beta_ci))) {
      sprintf(" (95%% CI %s, %s)", format_number(beta_ci[[1L]], rd), format_number(beta_ci[[2L]], rd))
    } else {
      ""
    }
    say(sprintf(
      "Using MAIVE (%s), the bias-corrected mean effect is %s%s, %s.",
      model_label, format_number(beta, rd), ci_text, verdict
    ))

    beta_standard <- maive_output$beta_standard
    if (is.numeric(beta_standard) && is.finite(beta_standard)) {
      say(sprintf(
        "The same model without the IV correction gives %s.",
        format_number(beta_standard, rd)
      ))
    }
  }

  # --- Publication bias ---
  egger_ci <- maive_output$egger_boot_ci
  if (!(is.numeric(egger_ci) && length(egger_ci) == 2L && all(is.finite(egger_ci)))) {
    egger_ci <- maive_ci(maive_output$egger_coef, maive_output$egger_se)
  }
  excludes <- ci_excludes_zero(egger_ci)
  if (!is.na(excludes)) {
    bias_kind <- if (identical(as.integer(options$instrument %||% 1L), 1L)) {
      "publication bias or p-hacking"
    } else {
      "publication bias"
    }
    say(sprintf(
      "We %s evidence of substantial %s (Egger 95%% CI %s 0).",
      if (excludes) "find" else "do not find",
      bias_kind,
      if (excludes) "excludes" else "includes"
    ))
  }

  # --- Diagnostics ---
  if (!identical(as.integer(options$instrument %||% 1L), 1L)) {
    return(sentences)
  }

  f_stat <- maive_first_stage_f(maive_output)
  strength <- instrument_strength(f_stat)
  if (!identical(strength, "unknown")) {
    say(sprintf(
      "The instrument is %s (%s = %s), so the Anderson-Rubin interval is %s.",
      strength,
      maive_first_stage_spec(resolved_first_stage$value)$f_phrase,
      format_number(f_stat, rd),
      if (identical(strength, "weak")) "the one to report" else "optional but recommended"
    ))
  }

  if (!identical(as.integer(options$studylevel %||% 2L), 1L)) {
    rejects <- hausman_rejects(maive_output$Hausman, maive_output$Chi2)
    if (is.na(rejects)) {
      say(
        "The Hausman test is uninformative here because the IV and OLS variances are ",
        "nearly identical."
      )
    } else {
      say(sprintf(
        "The Hausman test %s equality of OLS and IV, so evidence of spurious precision is %s.",
        if (rejects) "rejects" else "does not reject",
        if (rejects) "strong" else "moderate"
      ))
    }
  }

  if (is.finite(f_stat) && f_stat < LOG_FIRST_STAGE_F &&
    !identical(as.integer(resolved_first_stage$value), 1L)) {
    say(sprintf(
      "Because the first-stage F is below %s, running the first stage in logs (first_stage = 1) is recommended.",
      LOG_FIRST_STAGE_F
    ))
  }

  sentences
}

# Orchestration -------------------------------------------------------------

#' @title Run the MAIVE estimator and format its output
#' @description
#' Runs MAIVE, then builds the summary table and interpretation. Failures are
#' recorded as a skip reason rather than aborting, matching how the other
#' publication-bias methods degrade.
#' @param df *[data.frame]* Input data with effect, se, n_obs, and optionally
#'   study_id.
#' @param options *[list]* Resolved MAIVE options.
#' @return *[list]* With `summary`, `interpretation`, `first_stage`, `raw`, and
#'   `skipped`.
run_maive <- function(df, options) {
  validate(is.data.frame(df), is.list(options))

  if (!"n_obs" %in% colnames(df)) {
    return(list(
      summary = NULL, interpretation = character(), first_stage = NULL, raw = NULL,
      skipped = "requires the 'n_obs' column, which is absent from the data"
    ))
  }

  maive_data <- prepare_maive_data(df)
  first_stage <- resolve_maive_first_stage(options$first_stage %||% 0L, maive_data$Ns)

  if (identical(as.integer(options$se %||% 1L), 3L)) {
    cli::cli_alert_info("Running MAIVE with wild cluster bootstrap (this may take a moment)...")
  }

  self_env <- environment()
  failure_reason <- NULL
  raw <- tryCatch(
    maive(
      dat = maive_data,
      method = options$method,
      weight = options$weight,
      instrument = options$instrument,
      studylevel = options$studylevel,
      SE = options$se,
      AR = options$ar,
      first_stage = first_stage$value,
      seed = options$seed
    ),
    # conditionMessage(), not e$message: the latter keeps only the cli header,
    # dropping the bullets that carry the install and upgrade hints the user
    # needs to act on the skip.
    error = function(e) {
      self_env$failure_reason <- conditionMessage(e)
      NULL
    }
  )

  if (!is.null(failure_reason)) {
    return(list(
      summary = NULL, interpretation = character(), first_stage = first_stage, raw = NULL,
      skipped = failure_reason
    ))
  }

  data_info <- list(
    n_obs = nrow(df),
    n_studies = if ("study_id" %in% colnames(df)) length(unique(df$study_id)) else NULL
  )

  tryCatch(
    list(
      summary = build_maive_summary(raw, options, data_info, first_stage),
      interpretation = build_maive_interpretation(raw, options, first_stage),
      first_stage = first_stage,
      raw = raw,
      skipped = NULL
    ),
    error = function(e) {
      list(
        summary = NULL, interpretation = character(), first_stage = first_stage, raw = raw,
        skipped = paste("MAIVE result formatting failed:", conditionMessage(e))
      )
    }
  )
}

box::export(
  ar_ci_verdict,
  build_maive_interpretation,
  build_maive_summary,
  ci_excludes_zero,
  hausman_rejects,
  instrument_strength,
  maive_ci,
  maive_first_stage_f,
  maive_first_stage_is_weak,
  maive_first_stage_spec,
  maive_instrument_label,
  maive_method_label,
  maive_p_from_coef,
  maive_se_label,
  maive_studylevel_label,
  maive_weight_label,
  prepare_maive_data,
  resolve_maive_first_stage,
  run_maive
)
