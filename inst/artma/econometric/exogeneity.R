#' @title Exogeneity test helpers
#' @description Helper functions used by the exogeneity testing method.
NULL

# Not referenced directly: model fitters (e.g. ivreg) evaluate these in the
# formula environment, which is this module's environment.
box::use(
  stats[model.frame, pnorm, quantile]
)

box::use(
  artma / libs / formatting / results[
    significance_mark,
    format_number,
    format_se,
    format_ci
  ],
  artma / libs / core / validation[validate, assert]
)

# Robust variance-covariance matrix helper ---------------------------------

#' @title Get robust variance-covariance matrix with fallbacks
#' @description
#' Computes robust variance-covariance matrix using multiple fallback strategies
#' to avoid numerical instability warnings. Tries clustered standard errors first,
#' then falls back to non-clustered robust standard errors, and finally to standard
#' variance-covariance matrix.
#' @param model *[model object]* Regression model (e.g., from AER::ivreg).
#' @param cluster *[vector]* Clustering variable (e.g., study_id).
#' @return *[matrix]* Variance-covariance matrix.
get_robust_vcov <- function(model, cluster) {
  validate(!is.null(model), !is.null(cluster))

  # Try vcovCL with HC1 (clustered, most stable)
  vcov_result <- tryCatch(
    suppressWarnings(sandwich::vcovCL(model, cluster = cluster, type = "HC1")),
    error = function(e) NULL
  )

  if (!is.null(vcov_result)) {
    return(vcov_result)
  }

  # Fall back to vcovHC with HC1 (non-clustered, stable)
  vcov_result <- tryCatch(
    suppressWarnings(sandwich::vcovHC(model, type = "HC1")),
    error = function(e) NULL
  )

  if (!is.null(vcov_result)) {
    return(vcov_result)
  }

  # Fall back to vcovHC with HC0 (less stable but more robust)
  vcov_result <- tryCatch(
    suppressWarnings(sandwich::vcovHC(model, type = "HC0")),
    error = function(e) NULL
  )

  if (!is.null(vcov_result)) {
    return(vcov_result)
  }

  # Last resort: standard vcov
  stats::vcov(model)
}

# IV regression utilities --------------------------------------------------

#' @title Conventional threshold for the first-stage weak-instruments F-test
#' @description
#' Staiger-Stock/Stock-Yogo rule-of-thumb minimum first-stage F-statistic
#' below which an instrument is considered weak.
WEAK_INSTRUMENT_F_THRESHOLD <- 10

#' @title Default tie-break instrument among equally strong candidates
#' @description
#' `1/sqrt(n_obs)` is the theoretically motivated instrument for a
#' meta-analysis IV regression of effect on se, since the standard error
#' of an estimator scales with `1/sqrt(N)`. It is preferred whenever
#' several candidate instruments tie on first-stage strength.
WEAK_INSTRUMENT_TIEBREAK <- "1/sqrt(n_obs)"

#' @title Identify the strongest instrument for IV regression
#' @description
#' Ranks candidate instruments by first-stage strength: the "Weak
#' instruments" F-statistic reported by `AER::ivreg`'s diagnostics, which is
#' the standard weak-instruments diagnostic for a single endogenous
#' regressor. R-squared, Wu-Hausman, and Sargan are deliberately not used
#' for selection: IV R-squared is unbounded below and has no
#' instrument-quality interpretation; Wu-Hausman measures how strongly the
#' data reject exogeneity, a property of the data rather than the
#' instrument, so favoring a low Wu-Hausman p-value biases selection toward
#' concluding endogeneity; and Sargan is unidentified (df1 = 0, p = NA)
#' whenever there is exactly one instrument for one endogenous regressor,
#' the case here.
#' @param df *[data.frame]* Data frame with columns: effect, se, study_id, n_obs.
#' @param instruments *[list]* List of numeric vectors, each representing a potential instrument.
#' @param instruments_verbose *[character]* Verbose names for each instrument.
#' @return *[character]* Name of the strongest instrument by first-stage F-statistic.
find_best_instrument <- function(df, instruments, instruments_verbose) {
  validate(
    is.data.frame(df),
    is.list(instruments),
    is.character(instruments_verbose),
    length(instruments) == length(instruments_verbose)
  )

  required_cols <- c("effect", "se", "study_id", "n_obs")
  validate(all(required_cols %in% colnames(df)))

  first_stage_fstat <- rep(NA_real_, length(instruments))

  for (i in seq_along(instruments)) {
    instrument <- instruments[[i]]
    validate(is.numeric(instrument), length(instrument) == nrow(df))

    df$instr_temp <- instrument
    iv_formula <- stats::as.formula("effect ~ se | instr_temp")

    model <- tryCatch(
      AER::ivreg(formula = iv_formula, data = df),
      error = function(e) NULL
    )

    if (is.null(model)) {
      next
    }

    model_summary <- tryCatch(
      summary(model, vcov = get_robust_vcov(model, df$study_id), diagnostics = TRUE),
      error = function(e) NULL
    )

    if (is.null(model_summary) || is.null(model_summary$diagnostics)) {
      next
    }

    diag_names <- rownames(model_summary$diagnostics)
    if ("Weak instruments" %in% diag_names) {
      first_stage_fstat[i] <- model_summary$diagnostics["Weak instruments", "statistic"]
    }
  }

  names(first_stage_fstat) <- instruments_verbose

  assert(
    any(!is.na(first_stage_fstat)),
    "Unable to determine best instrument - first-stage F-statistic unavailable for all candidates"
  )

  max_fstat <- max(first_stage_fstat, na.rm = TRUE)
  best_instruments <- instruments_verbose[!is.na(first_stage_fstat) & first_stage_fstat == max_fstat]

  if (length(best_instruments) > 1 && WEAK_INSTRUMENT_TIEBREAK %in% best_instruments) {
    best_instruments <- WEAK_INSTRUMENT_TIEBREAK
  }

  best_instruments
}

#' @title Run IV regression with specified or automatic instrument
#' @description
#' Performs IV regression of effect on se using an instrumental variable.
#' Can automatically select the best instrument from a predefined set.
#' @param df *[data.frame]* Data frame with columns: effect, se, study_id, n_obs.
#' @param iv_instrument *[character]* Instrument specification or "automatic" for auto-selection.
#' @param add_significance_marks *[logical]* Whether to add significance asterisks.
#' @param round_to *[integer]* Number of decimal places for rounding.
#' @return *[list]* Contains coefficients, instrument name, and Anderson-Rubin F-statistic.
run_iv_regression <- function(df, iv_instrument = "automatic", add_significance_marks = TRUE, round_to = 3L) {
  validate(
    is.data.frame(df),
    is.character(iv_instrument),
    is.logical(add_significance_marks),
    is.numeric(round_to)
  )

  required_cols <- c("effect", "se", "study_id", "n_obs")
  validate(all(required_cols %in% colnames(df)))

  # Determine instrument
  if (iv_instrument == "automatic") {
    instruments <- list(
      1 / sqrt(df$n_obs),
      1 / df$n_obs,
      1 / df$n_obs^2,
      log(df$n_obs)
    )
    instruments_verbose <- c("1/sqrt(n_obs)", "1/n_obs", "1/n_obs^2", "log(n_obs)")

    best_instrument <- find_best_instrument(df, instruments, instruments_verbose)

    if (length(best_instrument) > 1) {
      best_instrument <- best_instrument[1]
    }

    best_instrument_values <- instruments[[match(best_instrument, instruments_verbose)]]
  } else {
    assert(grepl("n_obs", iv_instrument), "IV instrument must contain the column n_obs")
    best_instrument <- iv_instrument
    best_instrument_values <- eval(parse(text = gsub("n_obs", "df$n_obs", best_instrument)))
  }

  # Run IV regression
  df$instr_temp <- best_instrument_values
  iv_formula <- stats::as.formula("effect ~ se | instr_temp")
  model <- AER::ivreg(formula = iv_formula, data = df)
  model_summary <- summary(model, vcov = get_robust_vcov(model, df$study_id), diagnostics = TRUE)

  # Extract Anderson-Rubin F-statistic
  fstat <- tryCatch(
    {
      model_ar <- ivmodel::ivmodel(Y = df$effect, D = df$se, Z = df$instr_temp)
      model_ar$AR$Fstat
    },
    error = function(e) NA_real_
  )

  # First-stage F-statistic for the chosen instrument (weak-instruments diagnostic)
  first_stage_fstat <- NA_real_
  if (!is.null(model_summary$diagnostics) && "Weak instruments" %in% rownames(model_summary$diagnostics)) {
    first_stage_fstat <- model_summary$diagnostics["Weak instruments", "statistic"]
  }

  weak_instrument <- is.na(first_stage_fstat) || first_stage_fstat < WEAK_INSTRUMENT_F_THRESHOLD

  if (weak_instrument) {
    fstat_label <- format_number(first_stage_fstat, round_to)
    cli::cli_alert_warning(
      "Weak instrument: {.field {best_instrument}} has a first-stage F-statistic of {fstat_label} (below the conventional threshold of {WEAK_INSTRUMENT_F_THRESHOLD}). Publication-bias estimates from the IV regression may be unreliable."
    )
  }

  # Extract coefficients
  all_coefs <- model_summary$coefficients

  effect_est <- all_coefs["(Intercept)", "Estimate"]
  effect_se <- all_coefs["(Intercept)", "Std. Error"]
  effect_stat <- all_coefs["(Intercept)", "t value"]
  effect_p <- all_coefs["(Intercept)", "Pr(>|t|)"]

  pub_est <- all_coefs["se", "Estimate"]
  pub_se <- all_coefs["se", "Std. Error"]
  pub_stat <- all_coefs["se", "t value"]
  pub_p <- all_coefs["se", "Pr(>|t|)"]

  coefficients <- data.frame(
    term = c("effect", "publication_bias"),
    term_label = c("Effect Beyond Bias", "Publication Bias"),
    estimate = c(effect_est, pub_est),
    std_error = c(effect_se, pub_se),
    statistic = c(effect_stat, pub_stat),
    p_value = c(effect_p, pub_p),
    n_obs = nrow(df),
    stringsAsFactors = FALSE
  )

  coefficients$significance <- if (add_significance_marks) significance_mark(coefficients$p_value) else ""
  coefficients$estimate_formatted <- paste0(format_number(coefficients$estimate, round_to), coefficients$significance)
  coefficients$std_error_formatted <- format_se(coefficients$std_error, round_to)

  list(
    coefficients = coefficients,
    instrument_name = best_instrument,
    ar_fstat = fstat,
    first_stage_fstat = first_stage_fstat,
    weak_instrument = weak_instrument
  )
}

# p-uniform* implementation ------------------------------------------------

#' @title Compute study medians
#' @description
#' Computes the median value of a variable per study.
#' @param df *[data.frame]* Data frame containing study_id column.
#' @param var_name *[character]* Name of the variable to compute medians for.
#' @return *[numeric]* Vector of medians, one per study.
compute_study_medians <- function(df, var_name) {
  validate(
    is.data.frame(df),
    is.character(var_name),
    "study_id" %in% colnames(df),
    var_name %in% colnames(df)
  )

  splits <- split(df[[var_name]], df$study_id)
  medians <- vapply(splits, function(x) stats::median(x, na.rm = TRUE), numeric(1))
  medians
}

#' @title p-uniform* likelihood function
#' @description
#' Computes the negative log-likelihood for the p-uniform* model.
#' This is a local implementation to avoid dependency on the unstable puniform package.
#' @param params *[numeric]* Parameters (effect size, heterogeneity tau).
#' @param yi *[numeric]* Effect sizes.
#' @param vi *[numeric]* Variances.
#' @param ni *[numeric]* Sample sizes.
#' @param alpha *[numeric]* Significance level (default 0.05).
#' @return *[numeric]* Negative log-likelihood value.
puniform_star_nll <- function(params, yi, vi, ni, alpha = 0.05) {
  theta <- params[1]
  tau <- if (length(params) > 1) max(params[2], 0) else 0

  # Total variance
  vi_total <- vi + tau^2

  # Z-statistics under null
  zi <- yi / sqrt(vi_total)

  # Critical value
  z_crit <- stats::qnorm(1 - alpha / 2)

  # Publication probability (conditional on being significant)
  # We model only significant studies
  pub_prob <- 1 - stats::pnorm(z_crit, mean = theta / sqrt(vi_total), sd = 1) +
    stats::pnorm(-z_crit, mean = theta / sqrt(vi_total), sd = 1)

  # Likelihood contribution
  ll <- sum(stats::dnorm(yi, mean = theta, sd = sqrt(vi_total), log = TRUE) - log(pub_prob))

  -ll
}

#' @title Conditional p-value transform for the p-uniform selection model
#' @description
#' Computes, for each significant study, the CDF of its (sign-folded) z-score
#' conditional on being selected for statistical significance, evaluated at a
#' hypothesized true effect theta. Under the correctly specified theta these
#' values are distributed Uniform(0, 1) (van Assen, van Aert & Wicherts, 2015).
#' @param theta *[numeric]* Hypothesized true effect size.
#' @param yi *[numeric]* Effect sizes, restricted to significant studies.
#' @param vi *[numeric]* Variances.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[numeric]* Conditional p-values, one per study, in (0, 1).
puniform_transform <- function(theta, yi, vi, alpha) {
  sei <- sqrt(vi)
  z_crit <- stats::qnorm(1 - alpha / 2)
  sign_i <- sign(yi)
  zi <- sign_i * yi / sei
  ncp <- sign_i * theta / sei

  denom <- pmax(1 - stats::pnorm(z_crit - ncp), .Machine$double.eps)
  qi <- (stats::pnorm(zi - ncp) - stats::pnorm(z_crit - ncp)) / denom

  pmin(pmax(qi, .Machine$double.eps), 1 - .Machine$double.eps)
}

#' @title Method-of-moments estimation for p-uniform
#' @description
#' Estimates the true effect magnitude theta as the value for which the mean
#' conditional p-value (see puniform_transform) across significant studies
#' equals its expected value of 0.5, following the original p-uniform method
#' of van Assen, van Aert & Wicherts (2015). Standard errors use the delta
#' method; the publication-bias test statistic is Fisher's combined
#' probability test applied to the transform evaluated at theta = 0.
#' Because puniform_transform folds each study onto the sign of its own
#' effect, the objective is unimodal in theta with its peak near the data;
#' theta is therefore searched over non-negative values only, from the peak
#' (theta = 0) out to where the conditional p-value has decayed.
#' @param yi *[numeric]* Effect sizes, restricted to significant studies.
#' @param vi *[numeric]* Variances.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[list]* theta_est, theta_se, l_stat, l_pval.
run_puniform_mm <- function(yi, vi, alpha) {
  objective <- function(theta) mean(puniform_transform(theta, yi, vi, alpha)) - 0.5

  search_upper <- 2 * max(abs(yi)) + 10 * max(sqrt(vi))
  bounds_ok <- tryCatch(
    objective(0) * objective(search_upper) < 0,
    error = function(e) FALSE
  )

  theta_est <- if (isTRUE(bounds_ok)) {
    tryCatch(
      stats::uniroot(objective, lower = 0, upper = search_upper)$root,
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }

  theta_se <- if (is.finite(theta_est)) {
    tryCatch(
      {
        eps <- max(abs(theta_est), 1) * 1e-4
        deriv <- (objective(theta_est + eps) - objective(theta_est - eps)) / (2 * eps)
        sqrt(1 / (12 * length(yi))) / abs(deriv)
      },
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }

  qi_null <- puniform_transform(0, yi, vi, alpha)
  l_stat <- -2 * sum(log(qi_null))
  l_pval <- stats::pchisq(l_stat, df = 2 * length(yi), lower.tail = FALSE)

  list(
    theta_est = theta_est,
    theta_se = theta_se,
    l_stat = l_stat,
    l_pval = l_pval,
    converged = is.finite(theta_est),
    note = if (!is.finite(theta_est)) "P estimator: root not found within the search bounds; effect not estimable." else NULL
  )
}

#' @title Maximum-likelihood estimation for p-uniform*
#' @description
#' Fits the p-uniform* selection model by unconstrained maximum likelihood,
#' then tests for publication bias with a likelihood-ratio test against the
#' null of no effect (theta = 0). The null model must hold theta fixed at 0
#' and optimize only the heterogeneity parameter tau; starting the
#' unconstrained optimizer at theta = 0 is not a restriction; it converges
#' back to the same optimum as the full model and collapses the statistic to
#' zero.
#' @param yi *[numeric]* Effect sizes, restricted to significant studies.
#' @param vi *[numeric]* Variances.
#' @param ni *[numeric]* Sample sizes.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[list]* theta_est, theta_se, l_stat, l_pval, converged, note.
run_puniform_ml <- function(yi, vi, ni, alpha) {
  start_theta <- mean(yi)
  start_tau <- stats::sd(yi)

  opt_result <- tryCatch(
    stats::optim(
      par = c(start_theta, start_tau),
      fn = puniform_star_nll,
      yi = yi,
      vi = vi,
      ni = ni,
      alpha = alpha,
      method = "BFGS"
    ),
    error = function(e) list(par = c(NA_real_, NA_real_), value = NA_real_, convergence = 1)
  )

  if (opt_result$convergence != 0 || any(!is.finite(opt_result$par))) {
    return(list(
      theta_est = NA_real_,
      theta_se = NA_real_,
      l_stat = NA_real_,
      l_pval = NA_real_,
      converged = FALSE,
      note = sprintf("ML optimization did not converge (optim code %d).", opt_result$convergence)
    ))
  }

  theta_est <- opt_result$par[1]

  # Approximate standard error using the Hessian of the full model.
  theta_se <- tryCatch(
    {
      hess <- stats::optimHess(par = opt_result$par, fn = puniform_star_nll, yi = yi, vi = vi, ni = ni, alpha = alpha)
      se_val <- sqrt(solve(hess)[1, 1])
      if (is.finite(se_val)) se_val else NA_real_
    },
    error = function(e) NA_real_
  )

  # Likelihood-ratio test for publication bias (H0: theta = 0), holding theta
  # fixed at the null and optimizing only tau via a 1-D bounded search.
  ll_full <- -opt_result$value
  tau_search_upper <- max(start_tau, 1) * 10 + 10 * max(sqrt(vi))
  null_fit <- tryCatch(
    stats::optim(
      par = start_tau,
      fn = function(tau) puniform_star_nll(c(0, tau), yi, vi, ni, alpha),
      method = "Brent",
      lower = 0,
      upper = tau_search_upper
    ),
    error = function(e) NULL
  )

  if (is.null(null_fit) || !is.finite(null_fit$value)) {
    return(list(
      theta_est = theta_est,
      theta_se = theta_se,
      l_stat = NA_real_,
      l_pval = NA_real_,
      converged = TRUE,
      note = "ML null-model optimization (theta fixed at 0) failed; publication-bias test not computable."
    ))
  }

  ll_null <- -null_fit$value
  # Numerical noise in the two independent optimizations can occasionally put
  # the null log-likelihood a hair above the full model's; floor at 0 rather
  # than report a spurious negative statistic.
  l_stat <- max(2 * (ll_full - ll_null), 0)
  l_pval <- stats::pchisq(l_stat, df = 1, lower.tail = FALSE)

  list(
    theta_est = theta_est,
    theta_se = theta_se,
    l_stat = l_stat,
    l_pval = l_pval,
    converged = TRUE,
    note = if (is.na(theta_se)) "ML Hessian was not invertible; standard error is not computable." else NULL
  )
}

#' @title Run p-uniform* estimation
#' @description
#' Estimates publication bias and effect size using the p-uniform* method.
#' This is a local implementation based on van Aert & van Assen (2019),
#' supporting maximum likelihood ("ML") and method-of-moments ("P") estimation.
#' @param df *[data.frame]* Data frame with effect, se, study_id, study_size, n_obs.
#' @param add_significance_marks *[logical]* Whether to add significance asterisks.
#' @param round_to *[integer]* Number of decimal places for rounding.
#' @param alpha *[numeric]* Significance level for selection (default 0.05).
#' @param method *[character]* Estimation method ("ML" or "P").
#' @return *[list]* Contains coefficients, test statistics, the method actually
#'   used (`method_used`), and a `note` explaining non-convergence or a
#'   fallback from "ML" to "P", if either occurred.
run_puniform_star <- function(df, add_significance_marks = TRUE, round_to = 3L, alpha = 0.05, method = "ML") {
  validate(
    is.data.frame(df),
    is.logical(add_significance_marks),
    is.numeric(round_to),
    is.numeric(alpha),
    is.character(method)
  )
  assert(method %in% c("ML", "P"), "method must be one of 'ML' or 'P'.")

  required_cols <- c("effect", "se", "study_id", "study_size", "n_obs")
  validate(all(required_cols %in% colnames(df)))

  # Compute study medians
  med_yi <- compute_study_medians(df, "effect")
  med_ses <- compute_study_medians(df, "se")
  med_sample_sizes <- compute_study_medians(df, "n_obs")
  med_ni <- compute_study_medians(df, "study_size")

  # Compute variances
  med_sdi <- med_ses * sqrt(med_sample_sizes)
  med_vi <- med_sdi^2

  # Filter for significant effects (basic p-uniform assumption)
  z_scores <- abs(med_yi / med_ses)
  z_crit <- stats::qnorm(1 - alpha / 2)
  sig_mask <- z_scores >= z_crit

  if (sum(sig_mask) < 2) {
    # Not enough significant studies to estimate theta or run the LR test.
    # Fall through to the shared coefficient formatting below so the returned
    # data.frame always has the same columns as the fully-estimated case.
    theta_est <- NA_real_
    theta_se <- NA_real_
    l_stat <- NA_real_
    l_pval <- NA_real_
    method_used <- method
    note <- sprintf("Fewer than 2 studies were significant at alpha = %s; effect not estimable.", alpha)
  } else {
    yi_sig <- med_yi[sig_mask]
    vi_sig <- med_vi[sig_mask]
    ni_sig <- med_ni[sig_mask]

    fit_result <- if (method == "P") run_puniform_mm(yi_sig, vi_sig, alpha) else run_puniform_ml(yi_sig, vi_sig, ni_sig, alpha)
    method_used <- method

    # A failed ML fit falls back to the method-of-moments (P) estimator so a
    # single non-convergent optimization doesn't leave the whole test blank.
    if (method == "ML" && !isTRUE(fit_result$converged)) {
      fallback_result <- run_puniform_mm(yi_sig, vi_sig, alpha)
      ml_note <- fit_result$note %||% "ML estimation did not converge."
      if (isTRUE(fallback_result$converged)) {
        fit_result <- fallback_result
        method_used <- "P"
        fit_result$note <- paste(ml_note, "Fell back to the method-of-moments (P) estimator.")
      } else {
        fit_result$note <- ml_note
      }
    }

    theta_est <- fit_result$theta_est
    theta_se <- fit_result$theta_se
    l_stat <- fit_result$l_stat
    l_pval <- fit_result$l_pval
    note <- fit_result$note
  }

  # Format coefficients
  coefficients <- data.frame(
    term = c("effect", "publication_bias_test"),
    term_label = c("Effect Beyond Bias", "Publication Bias Test"),
    estimate = c(theta_est, l_stat),
    std_error = c(theta_se, NA_real_),
    statistic = c(if (is.finite(theta_est) && is.finite(theta_se) && theta_se > 0) theta_est / theta_se else NA_real_, l_stat),
    p_value = c(if (is.finite(theta_est) && is.finite(theta_se) && theta_se > 0) 2 * stats::pnorm(abs(theta_est / theta_se), lower.tail = FALSE) else NA_real_, l_pval),
    n_obs = nrow(df),
    stringsAsFactors = FALSE
  )

  coefficients$significance <- if (add_significance_marks) significance_mark(coefficients$p_value) else ""
  coefficients$estimate_formatted <- paste0(format_number(coefficients$estimate, round_to), coefficients$significance)
  coefficients$std_error_formatted <- format_se(coefficients$std_error, round_to)

  list(
    coefficients = coefficients,
    test_statistic = l_stat,
    test_p_value = l_pval,
    method_used = method_used,
    note = note
  )
}

# Main exogeneity test runner ----------------------------------------------

#' @title Run exogeneity tests
#' @description
#' Executes IV regression and p-uniform* tests to assess publication bias
#' and effect size under relaxed exogeneity assumptions.
#' @param df *[data.frame]* Input data.
#' @param options *[list]* Options containing iv_instrument, puniform settings, formatting.
#' @return *[list]* Contains coefficients and formatted summary.
run_exogeneity_tests <- function(df, options) {
  validate(is.data.frame(df), is.list(options))

  # Check for required packages
  if (!requireNamespace("AER", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg AER} is required for exogeneity tests. Install with: install.packages('AER')")
  }
  if (!requireNamespace("ivmodel", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ivmodel} is required for exogeneity tests. Install with: install.packages('ivmodel')")
  }

  required_cols <- c("effect", "se", "study_id", "n_obs", "study_size")
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    cli::cli_alert_warning("Missing required columns: {.field {missing_cols}}")
    return(list(
      coefficients = data.frame(),
      summary = data.frame(),
      skipped = list(reason = paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    ))
  }

  # Run IV regression
  iv_results <- tryCatch(
    run_iv_regression(
      df = df,
      iv_instrument = options$iv_instrument,
      add_significance_marks = options$add_significance_marks,
      round_to = options$round_to
    ),
    error = function(e) {
      list(
        coefficients = NULL,
        instrument_name = NA_character_,
        ar_fstat = NA_real_,
        first_stage_fstat = NA_real_,
        weak_instrument = NA,
        error = e$message
      )
    }
  )

  # Run p-uniform*
  puniform_results <- tryCatch(
    run_puniform_star(
      df = df,
      add_significance_marks = options$add_significance_marks,
      round_to = options$round_to,
      alpha = options$puniform_alpha,
      method = options$puniform_method
    ),
    error = function(e) {
      list(
        coefficients = NULL,
        test_statistic = NA_real_,
        test_p_value = NA_real_,
        error = e$message
      )
    }
  )

  # Build summary table
  summary <- build_exogeneity_summary(iv_results, puniform_results, options)

  list(
    iv = iv_results,
    puniform = puniform_results,
    summary = summary,
    options = options
  )
}

#' @title Build exogeneity tests summary table
#' @param iv_results *[list]* Results from IV regression.
#' @param puniform_results *[list]* Results from p-uniform* test.
#' @param options *[list]* Options.
#' @return *[data.frame]* Formatted summary table.
#' @title Placeholder for a metric that could not be computed
#' @description
#' Used in the exogeneity summary table wherever a coefficient, test
#' statistic, or p-value is unavailable, so the printed table reads legibly
#' instead of leaking raw `NA`/`<NA>` formatting.
NOT_COMPUTABLE <- "not computable"

#' @title Replace NA entries in a character vector with the placeholder
#' @param x *[character]* Vector to sanitize.
#' @return *[character]* Vector with `NA` values replaced by `NOT_COMPUTABLE`.
na_to_not_computable <- function(x) {
  x[is.na(x)] <- NOT_COMPUTABLE
  x
}

build_exogeneity_summary <- function(iv_results, puniform_results, options) {
  row_labels <- c(
    "Publication Bias",
    "(Std. Error)",
    "Effect Beyond Bias",
    "(Std. Error)",
    "Total Observations",
    "First-stage F",
    "F-test (AR)"
  )

  summary <- data.frame(
    Metric = row_labels,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # IV column
  if (!is.null(iv_results$coefficients)) {
    iv_coef <- iv_results$coefficients
    pb <- iv_coef[iv_coef$term == "publication_bias", , drop = FALSE]
    eff <- iv_coef[iv_coef$term == "effect", , drop = FALSE]

    first_stage_str <- format_number(iv_results$first_stage_fstat, options$round_to)
    if (isTRUE(iv_results$weak_instrument) && !is.na(first_stage_str)) {
      first_stage_str <- paste0(first_stage_str, " (weak instrument)")
    }

    summary[["IV"]] <- na_to_not_computable(c(
      if (nrow(pb) > 0) pb$estimate_formatted else NA_character_,
      if (nrow(pb) > 0) pb$std_error_formatted else NA_character_,
      if (nrow(eff) > 0) eff$estimate_formatted else NA_character_,
      if (nrow(eff) > 0) eff$std_error_formatted else NA_character_,
      if (nrow(iv_coef) > 0) format_number(iv_coef$n_obs[1], 0) else NA_character_,
      first_stage_str,
      format_number(iv_results$ar_fstat, options$round_to)
    ))
  } else {
    summary[["IV"]] <- rep(NOT_COMPUTABLE, length(row_labels))
  }

  # p-uniform* column
  if (!is.null(puniform_results$coefficients)) {
    pu_coef <- puniform_results$coefficients
    pb_test <- pu_coef[pu_coef$term == "publication_bias_test", , drop = FALSE]
    eff <- pu_coef[pu_coef$term == "effect", , drop = FALSE]

    # Format publication bias test as "L = X.XX (p = Y.YY)"
    pb_test_str <- if (nrow(pb_test) > 0 && is.finite(pb_test$statistic)) {
      paste0("L = ", format_number(pb_test$statistic, options$round_to))
    } else {
      NA_character_
    }

    pb_p_str <- if (nrow(pb_test) > 0 && is.finite(pb_test$p_value)) {
      paste0("(p = ", format_number(pb_test$p_value, options$round_to), ")")
    } else {
      NA_character_
    }

    summary[["p-Uniform*"]] <- na_to_not_computable(c(
      pb_test_str,
      pb_p_str,
      if (nrow(eff) > 0) eff$estimate_formatted else NA_character_,
      if (nrow(eff) > 0) eff$std_error_formatted else NA_character_,
      if (nrow(pu_coef) > 0) format_number(pu_coef$n_obs[1], 0) else NA_character_,
      "", # No first-stage F for p-uniform
      "" # No F-test for p-uniform
    ))
  } else {
    summary[["p-Uniform*"]] <- rep(NOT_COMPUTABLE, length(row_labels))
  }

  attr(summary, "row.names") <- row_labels
  summary
}

box::export(
  run_exogeneity_tests,
  run_iv_regression,
  run_puniform_star,
  find_best_instrument,
  WEAK_INSTRUMENT_F_THRESHOLD
)
