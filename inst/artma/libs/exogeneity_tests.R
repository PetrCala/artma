#' @title Exogeneity test helpers
#' @description Helper functions used by the exogeneity testing method.
NULL

box::use(
  stats[model.frame, pnorm, quantile]
)

box::use(
  artma / libs / result_formatters[
    significance_mark,
    format_number,
    format_se,
    format_ci
  ],
  artma / libs / validation[validate, assert]
)

# IV regression utilities --------------------------------------------------

#' @title Identify best instrument for IV regression
#' @description
#' Evaluates multiple instruments for IV regression using diagnostics
#' (R-squared, weak instruments test, Wu-Hausman test, Sargan test).
#' @param df *[data.frame]* Data frame with columns: effect, se, study_id, n_obs.
#' @param instruments *[list]* List of numeric vectors, each representing a potential instrument.
#' @param instruments_verbose *[character]* Verbose names for each instrument.
#' @return *[character]* Name(s) of the best instrument(s).
find_best_instrument <- function(df, instruments, instruments_verbose) {
  validate(
    is.data.frame(df),
    is.list(instruments),
    is.character(instruments_verbose),
    length(instruments) == length(instruments_verbose)
  )

  required_cols <- c("effect", "se", "study_id", "n_obs")
  validate(all(required_cols %in% colnames(df)))

  results <- data.frame(
    r_squared = numeric(length(instruments)),
    weak_instruments = numeric(length(instruments)),
    wu_hausman = numeric(length(instruments)),
    sargan = numeric(length(instruments))
  )

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
      results[i, ] <- NA_real_
      next
    }

    model_summary <- tryCatch(
      summary(model, vcov = sandwich::vcovHC(model, cluster = df$study_id), diagnostics = TRUE),
      error = function(e) NULL
    )

    if (is.null(model_summary)) {
      results[i, ] <- NA_real_
      next
    }

    results[i, "r_squared"] <- model_summary$r.squared

    if (!is.null(model_summary$diagnostics)) {
      diag_names <- rownames(model_summary$diagnostics)
      if ("Weak instruments" %in% diag_names) {
        results[i, "weak_instruments"] <- model_summary$diagnostics["Weak instruments", "p-value"]
      }
      if ("Wu-Hausman" %in% diag_names) {
        results[i, "wu_hausman"] <- model_summary$diagnostics["Wu-Hausman", "p-value"]
      }
      if ("Sargan" %in% diag_names) {
        results[i, "sargan"] <- model_summary$diagnostics["Sargan", "p-value"]
      }
    }
  }

  rownames(results) <- instruments_verbose

  # Determine best instrument based on frequency across metrics
  best_r_squared_idx <- if (any(!is.na(results$r_squared))) which.max(results$r_squared) else NA_integer_
  best_weak_instruments_idx <- if (any(!is.na(results$weak_instruments))) which.min(results$weak_instruments) else NA_integer_
  best_wu_hausman_idx <- if (any(!is.na(results$wu_hausman))) which.min(results$wu_hausman) else NA_integer_
  best_sargan_idx <- if (any(!is.na(results$sargan))) which.min(results$sargan) else NA_integer_

  best_instruments_idx <- c(best_r_squared_idx, best_weak_instruments_idx, best_wu_hausman_idx, best_sargan_idx)
  freqs <- table(best_instruments_idx[!is.na(best_instruments_idx)])

  assert(length(freqs) > 0, "Unable to determine best instrument - all diagnostics returned NA")

  max_freq <- max(freqs)
  max_values <- as.integer(names(freqs[freqs == max_freq]))
  best_instruments <- rownames(results)[max_values]

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
  model_summary <- summary(model, vcov = sandwich::vcovHC(model, cluster = df$study_id), diagnostics = TRUE)

  # Extract Anderson-Rubin F-statistic
  fstat <- tryCatch({
    model_ar <- ivmodel::ivmodel(Y = df$effect, D = df$se, Z = df$instr_temp)
    model_ar$AR$Fstat
  }, error = function(e) NA_real_)

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

  coefficients$significance <- if (add_significance_marks) vapply(coefficients$p_value, significance_mark, character(1)) else ""
  coefficients$estimate_formatted <- paste0(format_number(coefficients$estimate, round_to), coefficients$significance)
  coefficients$std_error_formatted <- vapply(coefficients$std_error, format_se, character(1), digits = round_to)

  list(
    coefficients = coefficients,
    instrument_name = best_instrument,
    ar_fstat = fstat
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

#' @title Run p-uniform* estimation
#' @description
#' Estimates publication bias and effect size using the p-uniform* method.
#' This is a local implementation based on van Aert & van Assen (2019).
#' @param df *[data.frame]* Data frame with effect, se, study_id, study_size, n_obs.
#' @param add_significance_marks *[logical]* Whether to add significance asterisks.
#' @param round_to *[integer]* Number of decimal places for rounding.
#' @param alpha *[numeric]* Significance level for selection (default 0.05).
#' @param method *[character]* Estimation method ("ML" or "P").
#' @return *[list]* Contains coefficients and test statistics.
run_puniform_star <- function(df, add_significance_marks = TRUE, round_to = 3L, alpha = 0.05, method = "ML") {
  validate(
    is.data.frame(df),
    is.logical(add_significance_marks),
    is.numeric(round_to),
    is.numeric(alpha),
    is.character(method)
  )

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
    # Not enough significant studies
    return(list(
      coefficients = data.frame(
        term = c("effect", "publication_bias_test"),
        term_label = c("Effect Beyond Bias", "Publication Bias Test"),
        estimate = c(NA_real_, NA_real_),
        std_error = c(NA_real_, NA_real_),
        statistic = c(NA_real_, NA_real_),
        p_value = c(NA_real_, NA_real_),
        n_obs = nrow(df),
        stringsAsFactors = FALSE
      ),
      test_statistic = NA_real_,
      test_p_value = NA_real_
    ))
  }

  yi_sig <- med_yi[sig_mask]
  vi_sig <- med_vi[sig_mask]
  ni_sig <- med_ni[sig_mask]

  # Optimize
  start_theta <- mean(yi_sig)
  start_tau <- stats::sd(yi_sig)

  opt_result <- tryCatch({
    stats::optim(
      par = c(start_theta, start_tau),
      fn = puniform_star_nll,
      yi = yi_sig,
      vi = vi_sig,
      ni = ni_sig,
      alpha = alpha,
      method = "BFGS"
    )
  }, error = function(e) {
    list(par = c(NA_real_, NA_real_), value = NA_real_, convergence = 1)
  })

  if (opt_result$convergence != 0 || any(is.na(opt_result$par))) {
    theta_est <- NA_real_
    theta_se <- NA_real_
    l_stat <- NA_real_
    l_pval <- NA_real_
  } else {
    theta_est <- opt_result$par[1]

    # Approximate standard error using Hessian
    theta_se <- tryCatch({
      hess <- stats::optimHess(
        par = opt_result$par,
        fn = puniform_star_nll,
        yi = yi_sig,
        vi = vi_sig,
        ni = ni_sig,
        alpha = alpha
      )
      sqrt(solve(hess)[1, 1])
    }, error = function(e) NA_real_)

    # Likelihood ratio test for publication bias (H0: theta = 0)
    ll_full <- -opt_result$value
    ll_null <- tryCatch({
      opt_null <- stats::optim(
        par = c(0, start_tau),
        fn = puniform_star_nll,
        yi = yi_sig,
        vi = vi_sig,
        ni = ni_sig,
        alpha = alpha,
        method = "BFGS"
      )
      -opt_null$value
    }, error = function(e) NA_real_)

    l_stat <- 2 * (ll_full - ll_null)
    l_pval <- if (is.finite(l_stat) && l_stat > 0) stats::pchisq(l_stat, df = 1, lower.tail = FALSE) else NA_real_
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

  coefficients$significance <- if (add_significance_marks) vapply(coefficients$p_value, significance_mark, character(1)) else ""
  coefficients$estimate_formatted <- paste0(format_number(coefficients$estimate, round_to), coefficients$significance)
  coefficients$std_error_formatted <- vapply(coefficients$std_error, format_se, character(1), digits = round_to)

  list(
    coefficients = coefficients,
    test_statistic = l_stat,
    test_p_value = l_pval
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
build_exogeneity_summary <- function(iv_results, puniform_results, options) {
  row_labels <- c(
    "Publication Bias",
    "(Std. Error)",
    "Effect Beyond Bias",
    "(Std. Error)",
    "Total Observations",
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

    summary[["IV"]] <- c(
      if (nrow(pb) > 0) pb$estimate_formatted else NA_character_,
      if (nrow(pb) > 0) pb$std_error_formatted else NA_character_,
      if (nrow(eff) > 0) eff$estimate_formatted else NA_character_,
      if (nrow(eff) > 0) eff$std_error_formatted else NA_character_,
      if (nrow(iv_coef) > 0) format_number(iv_coef$n_obs[1], 0) else NA_character_,
      format_number(iv_results$ar_fstat, options$round_to)
    )
  } else {
    summary[["IV"]] <- rep(NA_character_, length(row_labels))
  }

  # p-uniform* column
  if (!is.null(puniform_results$coefficients)) {
    pu_coef <- puniform_results$coefficients
    pb_test <- pu_coef[pu_coef$term == "publication_bias_test", , drop = FALSE]
    eff <- pu_coef[pu_coef$term == "effect", , drop = FALSE]

    # Format publication bias test as "L = X.XX (p = Y.YY)"
    pb_test_str <- if (nrow(pb_test) > 0) {
      paste0("L = ", format_number(pb_test$statistic, options$round_to))
    } else {
      NA_character_
    }

    pb_p_str <- if (nrow(pb_test) > 0) {
      paste0("(p = ", format_number(pb_test$p_value, options$round_to), ")")
    } else {
      NA_character_
    }

    summary[["p-Uniform*"]] <- c(
      pb_test_str,
      pb_p_str,
      if (nrow(eff) > 0) eff$estimate_formatted else NA_character_,
      if (nrow(eff) > 0) eff$std_error_formatted else NA_character_,
      if (nrow(pu_coef) > 0) format_number(pu_coef$n_obs[1], 0) else NA_character_,
      "" # No F-test for p-uniform
    )
  } else {
    summary[["p-Uniform*"]] <- rep(NA_character_, length(row_labels))
  }

  attr(summary, "row.names") <- row_labels
  summary
}

box::export(
  run_exogeneity_tests,
  run_iv_regression,
  run_puniform_star,
  find_best_instrument
)
