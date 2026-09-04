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
    format_estimate,
    format_se,
    format_ci
  ],
  artma / libs / formatting / summary_table[
    shared_build_summary_table = build_summary_table
  ],
  artma / libs / core / validation[validate, assert],
  artma / econometric / vcov[robust_vcov]
)

# Robust variance-covariance matrix helper ---------------------------------

#' @title Get robust variance-covariance matrix with fallbacks
#' @description
#' Thin wrapper over the shared [robust_vcov()] helper pinning this call site's
#' ladder: clustered HC1, then non-clustered HC1, then HC0, then `stats::vcov`,
#' with the robust steps run under `suppressWarnings()` and a required cluster.
#' @param model *[model object]* Regression model (e.g., from AER::ivreg).
#' @param cluster *[vector]* Clustering variable (e.g., study_id).
#' @return *[matrix]* Variance-covariance matrix.
get_robust_vcov <- function(model, cluster) {
  robust_vcov(
    model = model,
    cluster = cluster,
    engine = "sandwich",
    clustered_type = "HC1",
    fallback_types = c("HC1", "HC0"),
    require_cluster = TRUE,
    suppress_warnings = TRUE
  )
}

#' @title Clustered vcov as a function of the model
#' @description
#' `summary.ivreg` accepts either a vcov matrix or a function. Only the
#' function form reaches the diagnostic (weak-instruments) Wald tests: AER's
#' internal `wald()` falls back to the classical homoskedastic F whenever
#' `vcov.` is not a function. The function form also lets each (auxiliary)
#' model subset the cluster vector to its own model frame, so rows dropped
#' for missingness cannot silently break the clustered step.
#' @param df *[data.frame]* The data the models are fit on, with `study_id`.
#' @return *[function]* A one-argument function returning a vcov matrix.
make_cluster_vcov_fun <- function(df) {
  function(model) {
    idx <- match(rownames(stats::model.frame(model)), rownames(df))
    cluster <- df$study_id[idx]
    if (anyNA(cluster)) {
      cluster <- df$study_id
    }
    get_robust_vcov(model, cluster)
  }
}

# IV regression utilities --------------------------------------------------

#' @title Conventional threshold for the first-stage weak-instruments F-test
#' @description
#' Staiger-Stock/Stock-Yogo rule-of-thumb minimum first-stage F-statistic
#' below which an instrument is considered weak.
WEAK_INSTRUMENT_F_THRESHOLD <- 10

#' @title Coerce a test statistic to a single numeric value
#' @description
#' Test statistics pulled out of third-party model objects are occasionally
#' `NULL` or empty. Passing such a value on lets it vanish from a `c()` call
#' and silently shorten a summary column, so collapse it to `NA_real_` here.
#' @param x *[any]* Candidate statistic.
#' @return *[numeric(1)]* The statistic, or `NA_real_` if it is not a single number.
as_scalar_stat <- function(x) {
  if (!is.numeric(x) || length(x) != 1) {
    return(NA_real_)
  }
  x
}

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
#' regressor.
#'
#' This is reachable only via `iv_instrument = "automatic"` and is not the
#' default, because ranking instruments on the same data used for inference is
#' a specification search: it favors whichever instrument happens to look
#' strongest in sample and leaves the reported standard errors ignorant of the
#' selection step. The default, `1/sqrt(n_obs)`, is fixed a priori on the
#' grounds that an estimator's standard error scales with `1/sqrt(N)`.
#' Published meta-analyses are not unanimous here: `sqrt(n_obs)` and
#' `log(n_obs)` are both in common use and can be considerably stronger on a
#' given dataset, so the candidate set includes them.
#'
#' R-squared, Wu-Hausman, and Sargan are deliberately not used
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
      summary(model, vcov = make_cluster_vcov_fun(df), diagnostics = TRUE),
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
#' Defaults to `1/sqrt(n_obs)`, motivated by the estimator's standard error
#' scaling with `1/sqrt(N)`; `sqrt(n_obs)` and `log(n_obs)` are the other
#' specifications common in the applied literature. Can instead select from a
#' predefined set by first-stage strength, which is an exploratory option
#' rather than a sound default (see `find_best_instrument()`).
#' @param df *[data.frame]* Data frame with columns: effect, se, study_id, n_obs.
#' @param iv_instrument *[character]* Instrument specification, or "automatic" to
#'   select by first-stage strength.
#' @param add_significance_marks *[logical]* Whether to add significance asterisks.
#' @param round_to *[integer]* Number of decimal places for rounding.
#' @return *[list]* Contains coefficients, instrument name, and Anderson-Rubin F-statistic.
run_iv_regression <- function(df, iv_instrument = "1/sqrt(n_obs)", add_significance_marks = TRUE, round_to = 3L) {
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
      sqrt(df$n_obs),
      log(df$n_obs)
    )
    instruments_verbose <- c("1/sqrt(n_obs)", "1/n_obs", "1/n_obs^2", "sqrt(n_obs)", "log(n_obs)")

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

  # Run IV regression. The vcov function form (not a matrix) is what makes the
  # weak-instruments diagnostic cluster-robust, and t-based p-values use G - 1
  # degrees of freedom, the conventional reference for cluster-robust inference.
  df$instr_temp <- best_instrument_values
  iv_formula <- stats::as.formula("effect ~ se | instr_temp")
  n_clusters <- length(unique(df$study_id))
  model <- AER::ivreg(formula = iv_formula, data = df)
  model_summary <- summary(
    model,
    vcov = make_cluster_vcov_fun(df),
    df = max(n_clusters - 1L, 1L),
    diagnostics = TRUE
  )

  # Anderson-Rubin test of H0: no effect of se. With a single instrument this
  # is the significance of the reduced form (effect ~ instrument), computed
  # here with the study-clustered vcov; the classical AR F (previously taken
  # from `ivmodel`) assumes iid errors and overstates the statistic when
  # estimates share studies.
  fstat <- tryCatch(
    {
      reduced_form <- stats::lm(effect ~ instr_temp, data = df)
      rf_vcov <- make_cluster_vcov_fun(df)(reduced_form)
      rf_coef <- stats::coef(reduced_form)[["instr_temp"]]
      as_scalar_stat(rf_coef^2 / rf_vcov["instr_temp", "instr_temp"])
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
  coefficients$estimate_formatted <- format_estimate(coefficients$estimate, round_to, coefficients$significance)
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
#' Computes the negative log-likelihood for the p-uniform* selection model
#' (van Aert & van Assen, 2023). Every study contributes: a study that is
#' significant on the declared side is a normal density truncated to the
#' selection region, and a non-significant study is the same density truncated
#' to its complement. Fitting only the significant studies would be the
#' original p-uniform model (van Assen, van Aert & Wicherts, 2015); including
#' the non-significant ones is what the star variant adds. This is a local
#' implementation to avoid dependency on the unstable puniform package and
#' matches `puniform:::ml_star()`.
#'
#' Effects are expected in the "right-sided" frame: [run_puniform_star()]
#' flips the sign of the data for `side = "left"` before calling in here and
#' flips the estimate back afterwards.
#' @param params *[numeric]* Parameters (effect size, heterogeneity tau).
#' @param yi *[numeric]* Effect sizes of all studies, right-sided frame.
#' @param vi *[numeric]* Variances.
#' @param alpha *[numeric]* Significance level (default 0.05).
#' @return *[numeric]* Negative log-likelihood value.
puniform_star_nll <- function(params, yi, vi, alpha = 0.05) {
  theta <- params[1]
  tau <- if (length(params) > 1) max(params[2], 0) else 0

  # Total variance
  sd_total <- sqrt(vi + tau^2)

  # Selection is on the observed z-statistic yi / se_i, so the threshold in
  # effect space is z_crit * se_i, fixed in tau. Scaling the threshold by the
  # total SD instead lets the null model inflate tau until the observed
  # effects fall outside the selection region while it still divides by a
  # small P(significant), which spuriously rewards the null and collapses
  # the LR test.
  crit_y <- puniform_critical_z(alpha) * sqrt(vi)
  significant <- yi >= crit_y

  # Each study is conditioned on its own significance status, both on the log
  # scale so a theta far from the data does not underflow to log(0).
  log_dens <- stats::dnorm(yi, mean = theta, sd = sd_total, log = TRUE)
  log_p_sig <- stats::pnorm(crit_y, mean = theta, sd = sd_total, lower.tail = FALSE, log.p = TRUE)
  log_p_nsig <- stats::pnorm(crit_y, mean = theta, sd = sd_total, log.p = TRUE)
  log_p_status <- log_p_nsig
  log_p_status[significant] <- log_p_sig[significant]

  -sum(log_dens - log_p_status)
}

#' @title One-sided critical z-value of the p-uniform selection rule
#' @description
#' A study counts as significant on the declared side when its z-statistic
#' exceeds the two-sided `alpha` critical value in that direction, which is
#' the convention of `puniform::puni_star()` (its `alpha` is halved).
#' @param alpha *[numeric]* Two-sided significance level used for selection.
#' @return *[numeric]* Critical z-value.
puniform_critical_z <- function(alpha) {
  stats::qnorm(1 - alpha / 2)
}

#' @title Resolve the selection side of the p-uniform model
#' @description
#' Maps the user-facing `side` option to a sign multiplier. `"auto"` takes the
#' sign of the inverse-variance weighted mean of all studies; a pooled effect
#' of exactly zero is treated as right-sided.
#' @param side *[character]* One of `"auto"`, `"left"`, `"right"`.
#' @param yi *[numeric]* Effect sizes of all studies (original sign).
#' @param vi *[numeric]* Variances.
#' @return *[list]* `side` (the resolved side, never `"auto"`) and `sign`
#'   (`1` for right, `-1` for left).
resolve_puniform_side <- function(side, yi, vi) {
  if (side == "auto") {
    weights <- 1 / vi
    pooled <- sum(yi * weights) / sum(weights)
    side <- if (is.finite(pooled) && pooled < 0) "left" else "right"
  }
  list(side = side, sign = if (side == "left") -1 else 1)
}

#' @title Conditional p-value transform for the p-uniform selection model
#' @description
#' Computes, for each significant study, the CDF of its z-score conditional
#' on being selected for statistical significance, evaluated at a hypothesized
#' true effect theta. Under the correctly specified theta these values are
#' distributed Uniform(0, 1) (van Assen, van Aert & Wicherts, 2015). Inputs
#' are in the right-sided frame (see [run_puniform_star()]).
#' @param theta *[numeric]* Hypothesized true effect size.
#' @param yi *[numeric]* Effect sizes, restricted to significant studies.
#' @param vi *[numeric]* Variances.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[numeric]* Conditional p-values, one per study, in (0, 1).
puniform_transform <- function(theta, yi, vi, alpha) {
  sei <- sqrt(vi)
  z_crit <- puniform_critical_z(alpha)
  zi <- yi / sei
  ncp <- theta / sei

  # qi = (S(c) - S(z)) / S(c) = 1 - S(z)/S(c) with S the normal survival
  # function and z >= c. Computed on the log scale: the difference form
  # pnorm(z - ncp) - pnorm(c - ncp) cancels catastrophically once theta sits
  # far below the data (both terms saturate at 1), which turned the root
  # search's bracket endpoints into garbage.
  log_sz <- stats::pnorm(zi - ncp, lower.tail = FALSE, log.p = TRUE)
  log_sc <- stats::pnorm(z_crit - ncp, lower.tail = FALSE, log.p = TRUE)
  qi <- 1 - exp(log_sz - log_sc)

  pmin(pmax(qi, .Machine$double.eps), 1 - .Machine$double.eps)
}

#' @title Method-of-moments estimation for p-uniform
#' @description
#' Estimates the true effect theta as the value for which the mean
#' conditional p-value (see puniform_transform) across significant studies
#' equals its expected value of 0.5, following the original p-uniform method
#' of van Assen, van Aert & Wicherts (2015). Standard errors use the delta
#' method. The root is searched over a symmetric interval: the data arrive in
#' the right-sided frame, but a literature whose significant studies scatter
#' around the threshold can still place the root below zero, and a
#' non-negative search would leave it spuriously "not estimable".
#' @param yi *[numeric]* Effect sizes, restricted to significant studies.
#' @param vi *[numeric]* Variances.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[list]* theta_est, theta_se, converged, note.
run_puniform_mm <- function(yi, vi, alpha) {
  objective <- function(theta) mean(puniform_transform(theta, yi, vi, alpha)) - 0.5

  search_bound <- 2 * max(abs(yi)) + 10 * max(sqrt(vi))
  bounds_ok <- tryCatch(
    objective(-search_bound) * objective(search_bound) < 0,
    error = function(e) FALSE
  )

  theta_est <- if (isTRUE(bounds_ok)) {
    tryCatch(
      stats::uniroot(objective, lower = -search_bound, upper = search_bound)$root,
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

  list(
    theta_est = theta_est,
    theta_se = theta_se,
    converged = is.finite(theta_est),
    note = if (!is.finite(theta_est)) "P estimator: root not found within the search bounds; effect not estimable." else NULL
  )
}

#' @title Maximum-likelihood estimation for p-uniform*
#' @description
#' Fits the p-uniform* selection model by unconstrained maximum likelihood on
#' all studies, significant or not (see [puniform_star_nll()]).
#' The publication-bias test itself is method-independent and lives in
#' [run_puniform_star()]: the likelihood-ratio test of theta = 0 that used to
#' sit here was a test of no effect, not of publication bias, and was
#' reported under the wrong label.
#' @param yi *[numeric]* Effect sizes of all studies, right-sided frame.
#' @param vi *[numeric]* Variances.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[list]* theta_est, theta_se, converged, note.
run_puniform_ml <- function(yi, vi, alpha) {
  start_theta <- mean(yi)
  start_tau <- stats::sd(yi)

  opt_result <- tryCatch(
    stats::optim(
      par = c(start_theta, start_tau),
      fn = puniform_star_nll,
      yi = yi,
      vi = vi,
      alpha = alpha,
      method = "BFGS"
    ),
    error = function(e) list(par = c(NA_real_, NA_real_), value = NA_real_, convergence = 1)
  )

  if (opt_result$convergence != 0 || any(!is.finite(opt_result$par))) {
    return(list(
      theta_est = NA_real_,
      theta_se = NA_real_,
      converged = FALSE,
      note = sprintf("ML optimization did not converge (optim code %d).", opt_result$convergence)
    ))
  }

  theta_est <- opt_result$par[1]

  # Approximate standard error using the Hessian of the full model.
  theta_se <- tryCatch(
    {
      hess <- stats::optimHess(par = opt_result$par, fn = puniform_star_nll, yi = yi, vi = vi, alpha = alpha)
      se_val <- sqrt(solve(hess)[1, 1])
      if (is.finite(se_val)) se_val else NA_real_
    },
    error = function(e) NA_real_
  )

  list(
    theta_est = theta_est,
    theta_se = theta_se,
    converged = TRUE,
    note = if (is.na(theta_se)) "ML Hessian was not invertible; standard error is not computable." else NULL
  )
}

#' @title Fisher-type publication-bias test at the fixed-effect estimate
#' @description
#' The p-uniform publication-bias test (van Assen, van Aert & Wicherts, 2015):
#' evaluate the conditional p-value transform of the significant studies at
#' the ordinary fixed-effect estimate computed from ALL studies, and combine
#' with Fisher's method. Under no selection the transforms are Uniform(0, 1)
#' at the true effect, so a small p-value indicates an excess of
#' just-significant results, i.e. publication bias. Testing theta = 0 instead
#' (as an earlier version did) answers "is there an effect?", which flags
#' bias-free literatures with genuine effects as "biased".
#' @param yi_all *[numeric]* Effect sizes of all studies (per-study medians).
#' @param vi_all *[numeric]* Variances of all studies.
#' @param yi_sig *[numeric]* Effect sizes of the significant studies.
#' @param vi_sig *[numeric]* Variances of the significant studies.
#' @param alpha *[numeric]* Significance level used for selection.
#' @return *[list]* l_stat, l_pval, theta_fe.
run_puniform_bias_test <- function(yi_all, vi_all, yi_sig, vi_sig, alpha) {
  weights <- 1 / vi_all
  theta_fe <- sum(yi_all * weights) / sum(weights)

  qi <- puniform_transform(theta_fe, yi_sig, vi_sig, alpha)
  l_stat <- -2 * sum(log(qi))
  l_pval <- stats::pchisq(l_stat, df = 2 * length(yi_sig), lower.tail = FALSE)

  list(l_stat = l_stat, l_pval = l_pval, theta_fe = theta_fe)
}

#' @title Run p-uniform* estimation
#' @description
#' Estimates publication bias and effect size using the p-uniform* method.
#' This is a local implementation based on van Aert & van Assen (2023).
#' Selection is one-directional: a study is "significant" when its
#' z-statistic exceeds the two-sided `alpha` critical value on the declared
#' `side`, matching `puniform::puni_star()`. Internally the effects are
#' flipped into a right-sided frame, so every estimator below sees a
#' literature selected for significantly positive results, and the final
#' estimate is flipped back.
#'
#' Method `"ML"` fits the p-uniform* likelihood on all studies, significant
#' or not. Method `"P"` is the original p-uniform method-of-moments estimator
#' and uses the significant studies only.
#' @param df *[data.frame]* Data frame with effect, se, study_id.
#' @param add_significance_marks *[logical]* Whether to add significance asterisks.
#' @param round_to *[integer]* Number of decimal places for rounding.
#' @param alpha *[numeric]* Significance level for selection (default 0.05).
#' @param method *[character]* Estimation method ("ML" or "P").
#' @param side *[character]* Direction of the selection: `"left"`,
#'   `"right"`, or `"auto"` (default), which takes the sign of the
#'   inverse-variance weighted mean of all studies.
#' @return *[list]* Contains coefficients, test statistics, the method actually
#'   used (`method_used`), the resolved selection side (`side_used`), and a
#'   `note` explaining non-convergence or a fallback from "ML" to "P", if
#'   either occurred.
run_puniform_star <- function(df, add_significance_marks = TRUE, round_to = 3L, alpha = 0.05, method = "ML", side = "auto") {
  validate(
    is.data.frame(df),
    is.logical(add_significance_marks),
    is.numeric(round_to),
    is.numeric(alpha),
    is.character(method),
    is.character(side)
  )
  assert(method %in% c("ML", "P"), "method must be one of 'ML' or 'P'.")
  assert(side %in% c("auto", "left", "right"), "side must be one of 'auto', 'left', or 'right'.")

  required_cols <- c("effect", "se", "study_id")
  validate(all(required_cols %in% colnames(df)))

  # Compute study medians
  med_yi <- compute_study_medians(df, "effect")
  med_ses <- compute_study_medians(df, "se")

  # Sampling variance of each study's effect estimate. This must be se^2:
  # multiplying by the sample size (as an earlier version did) yields the
  # variance of the underlying micro observations, flattening the likelihood
  # and destroying the power of the LR test.
  med_vi <- med_ses^2

  # Flip into the right-sided frame so that "significant" means significantly
  # positive for every estimator below.
  resolved_side <- resolve_puniform_side(side, med_yi, med_vi)
  side_sign <- resolved_side$sign
  yi_all <- side_sign * med_yi

  # Studies significant on the declared side
  z_scores <- yi_all / med_ses
  sig_mask <- z_scores >= puniform_critical_z(alpha)

  if (sum(sig_mask) < 2) {
    # Not enough significant studies to estimate theta or run the bias test.
    # Fall through to the shared coefficient formatting below so the returned
    # data.frame always has the same columns as the fully-estimated case.
    theta_est <- NA_real_
    theta_se <- NA_real_
    l_stat <- NA_real_
    l_pval <- NA_real_
    method_used <- method
    note <- sprintf(
      "Fewer than 2 studies were significant at alpha = %s on the %s side; effect not estimable.",
      alpha, resolved_side$side
    )
  } else {
    yi_sig <- yi_all[sig_mask]
    vi_sig <- med_vi[sig_mask]

    fit_result <- if (method == "P") run_puniform_mm(yi_sig, vi_sig, alpha) else run_puniform_ml(yi_all, med_vi, alpha)
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

    # Back to the original sign; the standard error is sign-invariant.
    theta_est <- side_sign * fit_result$theta_est
    theta_se <- fit_result$theta_se
    note <- fit_result$note

    # The publication-bias test is method-independent: Fisher's method on the
    # conditional transforms at the all-studies fixed-effect estimate.
    bias_test <- run_puniform_bias_test(yi_all, med_vi, yi_sig, vi_sig, alpha)
    l_stat <- bias_test$l_stat
    l_pval <- bias_test$l_pval
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
  coefficients$estimate_formatted <- format_estimate(coefficients$estimate, round_to, coefficients$significance)
  coefficients$std_error_formatted <- format_se(coefficients$std_error, round_to)

  list(
    coefficients = coefficients,
    test_statistic = l_stat,
    test_p_value = l_pval,
    method_used = method_used,
    side_used = resolved_side$side,
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
#' @return *[list]* Contains coefficients and formatted summary. `skipped`
#'   carries a single string when the whole test suite could not run.
run_exogeneity_tests <- function(df, options) {
  validate(is.data.frame(df), is.list(options))

  # Check for required packages
  if (!requireNamespace("AER", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg AER} is required for exogeneity tests. Install with: install.packages('AER')")
  }

  required_cols <- c("effect", "se", "study_id", "n_obs", "study_size")
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    cli::cli_alert_warning("Missing required columns: {.field {missing_cols}}")
    return(list(
      coefficients = data.frame(),
      summary = data.frame(),
      skipped = paste("Missing columns:", paste(missing_cols, collapse = ", "))
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
      method = options$puniform_method,
      side = options$puniform_side %||% "auto"
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

  columns <- list()

  # IV column
  if (!is.null(iv_results$coefficients)) {
    iv_coef <- iv_results$coefficients
    pb <- iv_coef[iv_coef$term == "publication_bias", , drop = FALSE]
    eff <- iv_coef[iv_coef$term == "effect", , drop = FALSE]

    first_stage_str <- format_number(as_scalar_stat(iv_results$first_stage_fstat), options$round_to)
    if (isTRUE(iv_results$weak_instrument) && !is.na(first_stage_str)) {
      first_stage_str <- paste0(first_stage_str, " (weak instrument)")
    }

    columns[["IV"]] <- c(
      if (nrow(pb) > 0) pb$estimate_formatted else NA_character_,
      if (nrow(pb) > 0) pb$std_error_formatted else NA_character_,
      if (nrow(eff) > 0) eff$estimate_formatted else NA_character_,
      if (nrow(eff) > 0) eff$std_error_formatted else NA_character_,
      if (nrow(iv_coef) > 0) format_number(iv_coef$n_obs[1], 0) else NA_character_,
      first_stage_str,
      format_number(as_scalar_stat(iv_results$ar_fstat), options$round_to)
    )
  } else {
    columns[["IV"]] <- rep(NOT_COMPUTABLE, length(row_labels))
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

    columns[["p-Uniform*"]] <- c(
      pb_test_str,
      pb_p_str,
      if (nrow(eff) > 0) eff$estimate_formatted else NA_character_,
      if (nrow(eff) > 0) eff$std_error_formatted else NA_character_,
      if (nrow(pu_coef) > 0) format_number(pu_coef$n_obs[1], 0) else NA_character_,
      "", # No first-stage F for p-uniform
      "" # No F-test for p-uniform
    )
  } else {
    columns[["p-Uniform*"]] <- rep(NOT_COMPUTABLE, length(row_labels))
  }

  shared_build_summary_table(row_labels, columns, missing_value = NOT_COMPUTABLE)
}

box::export(
  build_exogeneity_summary,
  run_exogeneity_tests,
  run_iv_regression,
  run_puniform_star,
  run_puniform_bias_test,
  find_best_instrument,
  WEAK_INSTRUMENT_F_THRESHOLD
)
