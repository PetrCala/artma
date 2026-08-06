#' @title Non-linear model helpers
#' @description Helper utilities to run publication-bias corrections based on
#'   non-linear estimators.
NULL

box::use(
  stats[quantile],
  utils[capture.output],
  artma / libs / core / validation[validate, validate_columns, assert],
  artma / libs / formatting / results[
    format_number,
    format_estimate_with_pvalue,
    format_standard_error
  ],
  artma / calc / meta[normal_p_value],
  artma / libs / formatting / summary_table[
    shared_build_summary_table = build_summary_table
  ],
  artma / calc / methods / stem[stem, stem_funnel, stem_MSE, STEM_MIN_STUDIES],
  artma / calc / methods / selection_model[metastudies_estimation],
  artma / calc / methods / endo_kink[run_endogenous_kink],
  artma / econometric / vcov[robust_vcov],
  artma / visualization / options[get_visualization_options],
  artma / visualization / export[export_named_plots]
)

# nocov start -----------------------------------------------------------------

#' Kish's effective sample size for a set of precision weights
#'
#' @param weights *\[numeric\]* Non-negative weights (e.g. inverse-variance weights).
#' @return *\[numeric\]* The effective number of independent observations the
#'   weighted average behaves like. Equals `length(weights)` when weights are
#'   equal, and shrinks toward 1 when a single weight dominates the sum.
#' @keywords internal
effective_sample_size <- function(weights) {
  sum(weights)^2 / sum(weights^2)
}

#' Detect a degenerate effect/std-error pair produced by a nonlinear estimator
#'
#' @description
#' A term is degenerate when its estimate is not finite, or its standard
#' error is exactly zero or non-finite (typically a sign that a single
#' extreme-precision observation dominated the fit, or that an optimizer
#' returned a garbage covariance). A term that is genuinely absent by design
#' (an intentional `NA`, not a computed `NaN`) is left alone.
#'
#' @param component *\[list, optional\]* With elements `estimate` and `std_error`.
#' @param label *\[character\]* Human-readable name used in the skip reason.
#' @return *\[character or NULL\]* A skip reason, or `NULL` if not degenerate.
#' @keywords internal
degenerate_effect_reason <- function(component, label) {
  if (is.null(component)) {
    return(NULL)
  }
  estimate <- component$estimate
  std_error <- component$std_error
  if (is.na(std_error) && !is.nan(std_error)) {
    return(NULL)
  }
  if (!is.finite(estimate)) {
    return(paste0(label, " did not produce a finite estimate."))
  }
  if (!is.finite(std_error)) {
    return(paste0(label, " produced a non-finite standard error, indicating a failed optimizer or singular Hessian."))
  }
  if (std_error <= 0) {
    return(paste0(label, " produced a standard error of exactly zero, which is not a valid estimate."))
  }
  NULL
}

#' Flag WAAP/Top10 fits dominated by a single extreme-precision observation
#'
#' @param method_result *\[list\]* Runner output with `effective_n` and `n_model`.
#' @return *\[character or NULL\]* A skip reason, or `NULL` if not degenerate.
#' @keywords internal
degenerate_check_precision_weighted <- function(method_result) {
  eff_n <- method_result$effective_n
  if (is.null(eff_n) || !is.finite(eff_n) || eff_n >= 2) {
    return(NULL)
  }
  paste0(
    "the estimate is dominated by a single extreme-precision observation (effective sample size ",
    format_number(eff_n, 2), " across ", method_result$n_model, " model observations)."
  )
}

#' Flag STEM fits that land on its algorithmic minimum window
#'
#' @param method_result *\[list\]* Runner output with `n_model`.
#' @return *\[character or NULL\]* A skip reason, or `NULL` if not degenerate.
#' @keywords internal
degenerate_check_stem <- function(method_result) {
  n_model <- method_result$n_model
  if (is.null(n_model) || !is.finite(n_model) || n_model > STEM_MIN_STUDIES) {
    return(NULL)
  }
  paste0(
    "STEM selected only ", n_model, " studies, its algorithmic minimum of ", STEM_MIN_STUDIES,
    "; the fit is a corner solution and carries too little information to report."
  )
}

#' Flag selection-model fits that hit a boundary or failed to converge
#'
#' @param method_result *\[list\]* Runner output with `boundary_hit` and `convergence`.
#' @return *\[character or NULL\]* A skip reason, or `NULL` if not degenerate.
#' @keywords internal
degenerate_check_selection <- function(method_result) {
  if (isTRUE(method_result$boundary_hit)) {
    return("the publication-probability or heterogeneity parameter landed on its boundary (near zero), a corner solution rather than a genuine optimum.")
  }
  if (!is.null(method_result$convergence) && !identical(method_result$convergence, 0L) && !identical(method_result$convergence, 0)) {
    return(paste0("the optimizer did not converge (nlminb exit code ", method_result$convergence, ")."))
  }
  NULL
}

nonlinear_method_specs <- function(options) {
  list(
    list(
      name = "waap",
      label = "WAAP",
      runner = function(df, total_n) run_waap(df, total_n),
      degenerate_check = degenerate_check_precision_weighted
    ),
    list(
      name = "top10",
      label = "Top10",
      runner = function(df, total_n) run_top10(df, total_n),
      degenerate_check = degenerate_check_precision_weighted
    ),
    list(
      name = "stem",
      label = "Stem",
      runner = function(df, total_n) run_stem(df, total_n, options),
      degenerate_check = degenerate_check_stem
    ),
    list(
      name = "hierarchical",
      label = "Hierarch",
      runner = function(df, total_n) run_hierarchical(df, total_n, options)
    ),
    list(
      name = "selection",
      label = "Selection",
      runner = function(df, total_n) run_selection(df, total_n, options),
      degenerate_check = degenerate_check_selection
    ),
    list(
      name = "endogenous_kink",
      label = "Endogenous Kink",
      runner = function(df, total_n) run_endogenous(df, total_n)
    )
  )
}

#' Adequately-powered cutoff for the WAAP estimator
#'
#' @description
#' Following Ioannidis et al. (2017), the pilot (unrestricted) mean is the
#' inverse-variance weighted average of the effects; a study is adequately
#' powered when its standard error is below `|mean| / 2.8`.
#'
#' @param df *\[data.frame\]* With finite `effect` and positive `se` columns.
#' @return *\[numeric\]* The standard-error cutoff.
#' @keywords internal
waap_bound <- function(df) {
  weights <- 1 / df$se^2
  avg <- sum(df$effect * weights) / sum(weights)
  abs(avg) / 2.8
}

#' Keep the columns and rows WAAP/Top10 can use
#'
#' @description
#' Filters to finite `effect` and positive finite `se`, carrying `study_id`
#' along when the input has it so the fit can cluster by study.
#'
#' @param df *\[data.frame\]* Input data.
#' @return *\[data.frame\]* The cleaned subset.
#' @keywords internal
prepare_precision_weighted_data <- function(df) {
  validate_columns(df, c("effect", "se"))
  cols <- intersect(c("effect", "se", "study_id"), colnames(df))
  data <- df[, cols, drop = FALSE]
  data[is.finite(data$effect) & is.finite(data$se) & data$se > 0, , drop = FALSE]
}

#' Precision-weighted WLS fit shared by WAAP and Top10
#'
#' @description
#' Runs the WLS regression `t_stat ~ 0 + precision` on the selected
#' subsample. The coefficient equals the inverse-variance weighted mean of
#' the effects; the standard error is the regression standard error, which
#' scales with residual heterogeneity. It is clustered by `study_id` (HC1,
#' matching the linear tests) when clustering is possible, with a
#' non-clustered HC1 fallback.
#'
#' @param data *\[data.frame\]* With `effect`, `se`, and optionally `study_id`.
#' @return *\[list\]* With elements `estimate` and `std_error`.
#' @keywords internal
precision_weighted_wls <- function(data) {
  t_stat <- data$effect / data$se
  precision <- 1 / data$se
  model <- stats::lm(t_stat ~ 0 + precision)
  cluster <- data$study_id
  if (!is.null(cluster) && anyNA(cluster)) {
    cluster <- NULL
  }
  vcov <- robust_vcov(
    model = model,
    cluster = cluster,
    engine = "sandwich",
    clustered_type = "HC1",
    fallback_types = "HC1",
    final_vcov_fallback = FALSE
  )
  list(
    estimate = stats::coef(model)[["precision"]],
    std_error = sqrt(vcov["precision", "precision"])
  )
}

run_waap <- function(df, total_n) {
  data <- prepare_precision_weighted_data(df)
  if (nrow(data) < 2) {
    cli::cli_abort("Not enough observations to compute the WAAP estimator.")
  }
  bound <- waap_bound(data)
  if (!is.finite(bound) || bound <= 0) {
    cli::cli_abort("Failed to derive a finite WAAP bound.")
  }
  filtered <- data[data$se < bound, , drop = FALSE]
  if (nrow(filtered) < 2) {
    cli::cli_abort("Not enough adequately powered observations for the WAAP estimator.")
  }
  weights <- 1 / filtered$se^2
  fit <- precision_weighted_wls(filtered)
  list(
    effect = list(
      estimate = fit$estimate,
      std_error = fit$std_error,
      p_value = normal_p_value(fit$estimate, fit$std_error)
    ),
    n_model = nrow(filtered),
    effective_n = effective_sample_size(weights)
  )
}

run_top10 <- function(df, total_n) {
  data <- prepare_precision_weighted_data(df)
  if (nrow(data) < 2) {
    cli::cli_abort("Not enough observations to compute the Top10 estimator.")
  }
  precision <- 1 / data$se
  threshold <- quantile(precision, probs = 0.9, na.rm = TRUE, names = FALSE)
  filtered <- data[precision > threshold, , drop = FALSE]
  if (nrow(filtered) < 2) {
    cli::cli_abort("Not enough high-precision observations for the Top10 estimator.")
  }
  weights <- 1 / filtered$se^2
  fit <- precision_weighted_wls(filtered)
  list(
    effect = list(
      estimate = fit$estimate,
      std_error = fit$std_error,
      p_value = normal_p_value(fit$estimate, fit$std_error)
    ),
    n_model = nrow(filtered),
    effective_n = effective_sample_size(weights)
  )
}

summarise_by_study <- function(data, representative) {
  ids <- data$study_id
  if (representative == "medians") {
    effect <- tapply(data$effect, ids, stats::median, na.rm = TRUE)
    se <- tapply(data$se, ids, stats::median, na.rm = TRUE)
    return(list(effect = as.numeric(effect), se = as.numeric(se)))
  }
  if (representative == "first") {
    order_index <- order(ids)
    ordered <- data[order_index, , drop = FALSE]
    keep <- !duplicated(ordered$study_id)
    return(list(effect = ordered$effect[keep], se = ordered$se[keep]))
  }
  list(effect = data$effect, se = data$se)
}

run_stem <- function(df, total_n, options) {
  representative <- options$stem_representative_sample %||% "medians"
  valid_values <- c("medians", "first", "all")
  if (!representative %in% valid_values) {
    cli::cli_abort("Invalid STEM representative sample: {representative}.")
  }
  validate_columns(df, c("effect", "se", "study_id"))
  data <- df[, c("effect", "se", "study_id"), drop = FALSE]
  data <- data[is.finite(data$effect) & is.finite(data$se) & data$se > 0, , drop = FALSE]
  if (nrow(data) < 3) {
    cli::cli_abort("Not enough observations to run the STEM estimator.")
  }
  summary <- summarise_by_study(data, representative)
  effects <- summary$effect
  ses <- summary$se
  keep <- is.finite(effects) & is.finite(ses) & ses > 0
  effects <- effects[keep]
  ses <- ses[keep]
  if (length(effects) < 3) {
    cli::cli_abort("Not enough valid observations after summarising for the STEM estimator.")
  }
  param <- c(1e-4, 1e3)
  stem_fit <- stem(effects, ses, param)
  estimates <- stem_fit$estimates
  estimate <- estimates[1, "estimate"]
  std_error <- estimates[1, "se"]
  n_included <- as.integer(round(estimates[1, "n_stem"]))
  # STEM has no standard error for its heterogeneity estimate; NA (not NaN)
  # is the documented "absent by design" signal degenerate_effect_reason()
  # and the formatters already understand.
  heterogeneity_estimate <- estimates[1, "sd of total heterogeneity"]
  extra_terms <- list(list(
    term = "effect_heterogeneity",
    term_label = "Effect Heterogeneity (tau)",
    estimate = heterogeneity_estimate,
    std_error = NA_real_,
    p_value = NA_real_
  ))
  list(
    effect = list(
      estimate = estimate,
      std_error = std_error,
      p_value = normal_p_value(estimate, std_error)
    ),
    extra_terms = extra_terms,
    n_model = n_included,
    plots = build_stem_plots(effects, ses, estimates, stem_fit$MSE)
  )
}

#' Build the STEM funnel and MSE diagnostic plots
#'
#' @param effects *\[numeric\]* Effect sizes used to fit the STEM estimator.
#' @param ses *\[numeric\]* Standard errors used to fit the STEM estimator.
#' @param estimates *\[matrix\]* The `stem()` estimates matrix.
#' @param mse_matrix *\[matrix\]* The `stem()` MSE matrix.
#' @return *\[list\]* With elements `stem_funnel` and `stem_mse`, each either a
#'   `recordedplot` object or `NULL` if the plot could not be built.
#' @keywords internal
build_stem_plots <- function(effects, ses, estimates, mse_matrix) {
  tryCatch(
    {
      vis <- get_visualization_options()
      stem_estimates <- as.numeric(estimates[1, c("estimate", "se", "sd of total heterogeneity", "n_stem")])

      draw_funnel <- function() stem_funnel(effects, ses, stem_estimates, vis$theme)
      draw_mse <- function() stem_MSE(mse_matrix)

      if (isTRUE(vis$export_graphics)) {
        export_named_plots(
          plots = list(funnel = draw_funnel, mse = draw_mse),
          base_name = "stem",
          export_path = vis$export_path,
          graph_scale = vis$graph_scale,
          width = 800,
          height = 600,
          renderer = "base"
        )
      }

      list(
        stem_funnel = record_stem_plot(draw_funnel),
        stem_mse = record_stem_plot(draw_mse)
      )
    },
    error = function(e) {
      cli::cli_warn("Could not build STEM diagnostic plots: {conditionMessage(e)}")
      list(stem_funnel = NULL, stem_mse = NULL)
    }
  )
}

#' Render a base-graphics plotting function into a recorded plot object
#'
#' @param draw *\[function\]* Zero-argument function that draws the plot.
#' @return *\[recordedplot\]* The recorded plot.
#' @keywords internal
record_stem_plot <- function(draw) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grDevices::dev.control("enable")
  draw()
  grDevices::recordPlot()
}

run_hierarchical <- function(df, total_n, options) {
  if (!requireNamespace("bayesm", quietly = TRUE)) {
    cli::cli_abort("Package 'bayesm' is required to run the hierarchical model.")
  }
  validate_columns(df, c("effect", "se", "study_id"))
  data <- df[, c("effect", "se", "study_id"), drop = FALSE]
  data <- data[is.finite(data$effect) & is.finite(data$se) & !is.na(data$study_id), , drop = FALSE]
  if (nrow(data) < 2) {
    cli::cli_abort("Not enough observations to run the hierarchical model.")
  }
  study_ids <- droplevels(factor(data$study_id))
  regdata <- lapply(levels(study_ids), function(id) {
    mask <- study_ids == id
    list(
      y = data$effect[mask],
      X = cbind(1, data$se[mask])
    )
  })
  iterations <- options$hierarchical_iterations %||% 6000L
  iterations <- as.integer(iterations)
  assert(iterations > 0, "Hierarchical iterations must be positive.")
  fit <- suppressWarnings({
    result <- NULL
    capture.output(
      result <- bayesm::rhierLinearModel(
        Data = list(regdata = regdata),
        Mcmc = list(R = iterations, nprint = 0L)
      ),
      type = "output"
    )
    result
  })
  draws <- fit$Deltadraw
  vbeta_draws <- fit$Vbetadraw
  if (is.null(draws) || ncol(draws) < 2 || is.null(vbeta_draws) || ncol(vbeta_draws) < 1) {
    cli::cli_abort("Unexpected posterior output from the hierarchical model.")
  }
  effect_draws <- draws[, 1]
  pub_bias_draws <- draws[, 2]
  effect_est <- mean(effect_draws)
  effect_se <- stats::sd(effect_draws)
  pub_bias_est <- mean(pub_bias_draws)
  pub_bias_se <- stats::sd(pub_bias_draws)
  # Vbetadraw holds posterior draws of vec(Vbeta), the nvar x nvar covariance
  # matrix of the cross-study coefficients (column-major, mirroring
  # Deltadraw's column order); column 1 is therefore Var(intercept), i.e. the
  # cross-study heterogeneity of the effect. sqrt() of each draw gives
  # posterior draws of the heterogeneity SD, whose mean/sd form an
  # estimate/SE pair the same way effect_est/effect_se come from
  # mean()/sd() of Deltadraw's column.
  heterogeneity_sd_draws <- sqrt(pmax(vbeta_draws[, 1], 0))
  heterogeneity_est <- mean(heterogeneity_sd_draws)
  heterogeneity_se <- stats::sd(heterogeneity_sd_draws)
  extra_terms <- list(list(
    term = "effect_heterogeneity",
    term_label = "Effect Heterogeneity (tau)",
    estimate = heterogeneity_est,
    std_error = heterogeneity_se,
    p_value = normal_p_value(heterogeneity_est, heterogeneity_se)
  ))
  list(
    effect = list(
      estimate = effect_est,
      std_error = effect_se,
      p_value = normal_p_value(effect_est, effect_se)
    ),
    publication_bias = list(
      estimate = pub_bias_est,
      std_error = pub_bias_se,
      p_value = normal_p_value(pub_bias_est, pub_bias_se)
    ),
    extra_terms = extra_terms,
    n_model = nrow(data)
  )
}

#' Label the t-statistic intervals of the selection model's publication
#' probabilities
#'
#' @param cutoffs *\[numeric\]* Cut-off thresholds, strictly increasing.
#' @param symmetric *\[logical\]* Whether the model applies cutoffs to `|t|`.
#' @return *\[character\]* One label per estimated publication-probability
#'   parameter; the interval above the last cutoff is the reference category
#'   with its probability normalised to 1.
#' @keywords internal
selection_interval_labels <- function(cutoffs, symmetric) {
  bounds <- c(if (symmetric) "0" else "-Inf", as.character(cutoffs))
  vapply(seq_along(cutoffs), function(i) {
    left <- if (i == 1 && symmetric) "[" else "("
    paste0(left, bounds[i], ", ", bounds[i + 1], "]")
  }, character(1))
}

run_selection <- function(df, total_n, options) {
  validate_columns(df, c("effect", "se"))
  columns <- c("effect", "se")
  has_study_id <- "study_id" %in% colnames(df)
  if (has_study_id) {
    columns <- c(columns, "study_id")
  }
  data <- df[, columns, drop = FALSE]
  data <- data[is.finite(data$effect) & is.finite(data$se) & data$se > 0, , drop = FALSE]
  if (nrow(data) < 2) {
    cli::cli_abort("Not enough observations to run the selection model.")
  }
  cutoffs <- options$selection_cutoffs %||% c(1.96)
  symmetric <- options$selection_symmetric %||% FALSE
  model <- options$selection_model %||% "normal"
  estimates <- metastudies_estimation(
    X = data$effect,
    sigma = data$se,
    cutoffs = cutoffs,
    symmetric = symmetric,
    model = model,
    cluster_id = if (has_study_id) data$study_id else NULL
  )
  n_shape_params <- if (model == "t") 3L else 2L
  n_params <- n_shape_params + length(cutoffs)
  if (length(estimates$Psihat) != n_params || length(estimates$SE) != n_params) {
    cli::cli_abort("Selection model did not return the expected parameter vector.")
  }
  effect_est <- estimates$Psihat[1]
  effect_se <- estimates$SE[1]
  tau_est <- estimates$Psihat[2]
  tau_se <- estimates$SE[2]
  extra_terms <- list(list(
    term = "effect_heterogeneity",
    term_label = "Effect Heterogeneity (tau)",
    estimate = tau_est,
    std_error = tau_se,
    p_value = normal_p_value(tau_est, tau_se)
  ))
  interval_labels <- selection_interval_labels(cutoffs, symmetric)
  pub_prob_index <- seq.int(n_shape_params + 1L, n_params)
  for (i in seq_along(pub_prob_index)) {
    pub_prob_est <- estimates$Psihat[pub_prob_index[i]]
    pub_prob_se <- estimates$SE[pub_prob_index[i]]
    extra_terms[[i + 1L]] <- list(
      term = paste0("pub_prob_", i),
      term_label = paste0("Rel. Pub. Probability ", interval_labels[i]),
      estimate = pub_prob_est,
      std_error = pub_prob_se,
      # Publication probabilities are relative to the reference interval above
      # the last cutoff, so the no-selection null is 1, not 0.
      p_value = normal_p_value(pub_prob_est - 1, pub_prob_se)
    )
  }
  list(
    effect = list(
      estimate = effect_est,
      std_error = effect_se,
      p_value = normal_p_value(effect_est, effect_se)
    ),
    extra_terms = extra_terms,
    n_model = nrow(data),
    convergence = estimates$convergence,
    boundary_hit = estimates$boundary_hit
  )
}

run_endogenous <- function(df, total_n) {
  validate_columns(df, c("effect", "se"))
  data <- df[, c("effect", "se"), drop = FALSE]
  data <- data[is.finite(data$effect) & is.finite(data$se) & data$se > 0, , drop = FALSE]
  if (nrow(data) < 2) {
    cli::cli_abort("Not enough observations to run the endogenous kink model.")
  }
  estimates <- run_endogenous_kink(data, verbose = FALSE)
  if (length(estimates) < 5) {
    cli::cli_abort("Endogenous kink model did not return the expected coefficients.")
  }
  effect_est <- estimates[1]
  effect_se <- estimates[2]
  pub_bias_est <- estimates[3]
  pub_bias_se <- estimates[4]
  heterogeneity_estimate <- estimates[5]
  # No standard error is computed for the heterogeneity SD; NA (not NaN) is
  # the documented "absent by design" signal degenerate_effect_reason() and
  # the formatters already understand.
  extra_terms <- list(list(
    term = "effect_heterogeneity",
    term_label = "Effect Heterogeneity (tau)",
    estimate = heterogeneity_estimate,
    std_error = NA_real_,
    p_value = NA_real_
  ))
  list(
    effect = list(
      estimate = effect_est,
      std_error = effect_se,
      p_value = normal_p_value(effect_est, effect_se)
    ),
    publication_bias = list(
      estimate = pub_bias_est,
      std_error = pub_bias_se,
      p_value = normal_p_value(pub_bias_est, pub_bias_se)
    ),
    extra_terms = extra_terms,
    n_model = nrow(data)
  )
}

build_summary_table <- function(coefficients, digits) {
  if (!nrow(coefficients)) {
    return(data.frame())
  }
  is_extra <- !coefficients$term %in% c("publication_bias", "effect")
  extra_labels <- unique(coefficients$term_label[is_extra])
  extra_row_labels <- as.vector(rbind(extra_labels, rep("(Std. Error)", length(extra_labels))))
  row_labels <- c(
    "Publication Bias",
    "(Std. Error)",
    "Effect Beyond Bias",
    "(Std. Error)",
    extra_row_labels,
    "Total observations",
    "Model observations"
  )
  columns <- list()
  for (model in unique(coefficients$model)) {
    model_rows <- coefficients[coefficients$model == model, , drop = FALSE]
    pb_row <- model_rows[model_rows$term == "publication_bias", , drop = FALSE]
    eff_row <- model_rows[model_rows$term == "effect", , drop = FALSE]
    extra_cells <- character(0)
    for (extra_label in extra_labels) {
      extra_row <- model_rows[model_rows$term_label == extra_label & !model_rows$term %in% c("publication_bias", "effect"), , drop = FALSE]
      extra_cells <- c(
        extra_cells,
        if (nrow(extra_row)) extra_row$estimate_formatted else "",
        if (nrow(extra_row)) extra_row$std_error_formatted else ""
      )
    }
    total_obs <- unique(model_rows$n_obs_total)
    total_obs <- total_obs[is.finite(total_obs)]
    model_obs <- unique(model_rows$n_obs_model)
    model_obs <- model_obs[is.finite(model_obs)]
    columns[[model_rows$model_label[1]]] <- c(
      if (nrow(pb_row)) pb_row$estimate_formatted else "",
      if (nrow(pb_row)) pb_row$std_error_formatted else "",
      if (nrow(eff_row)) eff_row$estimate_formatted else "",
      if (nrow(eff_row)) eff_row$std_error_formatted else "",
      extra_cells,
      if (length(total_obs)) format_number(total_obs[1], 0) else "",
      if (length(model_obs)) format_number(model_obs[1], 0) else ""
    )
  }
  shared_build_summary_table(row_labels, columns, missing_value = "")
}

run_nonlinear_methods <- function(df, options) {
  validate(is.data.frame(df))
  total_n <- nrow(df)
  specs <- nonlinear_method_specs(options)
  results <- list()
  skipped <- list()
  plots <- list()
  for (spec in specs) {
    tryCatch(
      {
        method_result <- spec$runner(df, total_n)
        degenerate_reason <- degenerate_effect_reason(method_result$effect, "Effect estimate")
        if (is.null(degenerate_reason)) {
          degenerate_reason <- degenerate_effect_reason(method_result$publication_bias, "Publication-bias estimate")
        }
        if (is.null(degenerate_reason)) {
          for (extra in method_result$extra_terms %||% list()) {
            degenerate_reason <- degenerate_effect_reason(extra, paste(extra$term_label, "estimate"))
            if (!is.null(degenerate_reason)) break
          }
        }
        if (is.null(degenerate_reason) && !is.null(spec$degenerate_check)) {
          degenerate_reason <- spec$degenerate_check(method_result)
        }
        if (!is.null(degenerate_reason)) {
          cli::cli_abort(degenerate_reason)
        }
        coefficients <- list()
        pb <- method_result$publication_bias %||% list(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_)
        effect <- method_result$effect
        n_model <- method_result$n_model %||% total_n
        if (!is.null(method_result$plots)) {
          plots[[spec$name]] <- method_result$plots
        }
        coefficients[[1]] <- data.frame(
          model = spec$name,
          model_label = spec$label,
          term = "publication_bias",
          term_label = "Publication Bias",
          estimate = pb$estimate,
          std_error = pb$std_error,
          p_value = pb$p_value,
          n_obs_total = total_n,
          n_obs_model = n_model,
          stringsAsFactors = FALSE
        )
        coefficients[[2]] <- data.frame(
          model = spec$name,
          model_label = spec$label,
          term = "effect",
          term_label = "Effect Beyond Bias",
          estimate = effect$estimate,
          std_error = effect$std_error,
          p_value = effect$p_value,
          n_obs_total = total_n,
          n_obs_model = n_model,
          stringsAsFactors = FALSE
        )
        for (extra in method_result$extra_terms %||% list()) {
          coefficients[[length(coefficients) + 1]] <- data.frame(
            model = spec$name,
            model_label = spec$label,
            term = extra$term,
            term_label = extra$term_label,
            estimate = extra$estimate,
            std_error = extra$std_error,
            p_value = extra$p_value,
            n_obs_total = total_n,
            n_obs_model = n_model,
            stringsAsFactors = FALSE
          )
        }
        results[[length(results) + 1]] <- do.call(rbind, coefficients)
      },
      error = function(e) {
        skipped[[spec$name]] <<- list(label = spec$label, reason = e$message)
      }
    )
  }
  if (!length(results)) {
    empty <- data.frame(
      model = character(),
      model_label = character(),
      term = character(),
      term_label = character(),
      estimate = numeric(),
      std_error = numeric(),
      p_value = numeric(),
      n_obs_total = numeric(),
      n_obs_model = numeric(),
      stringsAsFactors = FALSE
    )
    return(list(coefficients = empty, summary = empty, skipped = skipped, options = options, plots = plots))
  }
  coefficients <- do.call(rbind, results)
  digits <- options$round_to %||% 3L
  add_marks <- isTRUE(options$add_significance_marks)
  coefficients$estimate_formatted <- format_estimate_with_pvalue(coefficients$estimate, coefficients$p_value, digits, add_marks)
  coefficients$std_error_formatted <- format_standard_error(coefficients$std_error, digits)
  summary <- build_summary_table(coefficients, digits)
  list(
    coefficients = coefficients,
    summary = summary,
    skipped = skipped,
    options = options,
    plots = plots
  )
}

box::export(
  run_nonlinear_methods,
  run_waap,
  run_top10,
  waap_bound
)

# nocov end -------------------------------------------------------------------
