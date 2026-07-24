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
  artma / calc / methods / stem[stem, stem_funnel, stem_MSE, STEM_MIN_STUDIES],
  artma / calc / methods / selection_model[metastudies_estimation],
  artma / calc / methods / endo_kink[run_endogenous_kink],
  artma / visualization / options[get_visualization_options],
  artma / visualization / export[ensure_export_dir, build_export_filename, open_png_device]
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

prepare_basic_data <- function(df, required_cols) {
  validate_columns(df, required_cols)
  cleaned <- df[, required_cols, drop = FALSE]
  for (col in required_cols) {
    cleaned <- cleaned[is.finite(cleaned[[col]]), , drop = FALSE]
  }
  cleaned
}

waap_bound <- function(df) {
  weights <- 1 / df$se
  avg <- sum(df$effect * weights) / sum(weights)
  abs(avg) / 2.8
}

run_waap <- function(df, total_n) {
  data <- prepare_basic_data(df, c("effect", "se"))
  data <- data[data$se > 0, , drop = FALSE]
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
  estimate <- sum(filtered$effect * weights) / sum(weights)
  std_error <- sqrt(1 / sum(weights))
  list(
    effect = list(
      estimate = estimate,
      std_error = std_error,
      p_value = normal_p_value(estimate, std_error)
    ),
    n_model = nrow(filtered),
    effective_n = effective_sample_size(weights)
  )
}

run_top10 <- function(df, total_n) {
  data <- prepare_basic_data(df, c("effect", "se"))
  data <- data[data$se > 0, , drop = FALSE]
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
  estimate <- sum(filtered$effect * weights) / sum(weights)
  std_error <- sqrt(1 / sum(weights))
  list(
    effect = list(
      estimate = estimate,
      std_error = std_error,
      p_value = normal_p_value(estimate, std_error)
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
  list(
    effect = list(
      estimate = estimate,
      std_error = std_error,
      p_value = normal_p_value(estimate, std_error)
    ),
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
        ensure_export_dir(vis$export_path)
        export_stem_plot(draw_funnel, file.path(vis$export_path, build_export_filename("stem", "funnel")), vis$graph_scale)
        export_stem_plot(draw_mse, file.path(vis$export_path, build_export_filename("stem", "mse")), vis$graph_scale)
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

#' Export a base-graphics plotting function to a PNG file
#'
#' @param draw *\[function\]* Zero-argument function that draws the plot.
#' @param path *\[character\]* Full file path (including filename).
#' @param graph_scale *\[numeric\]* Scale factor for the exported dimensions.
#' @return NULL (invisibly)
#' @keywords internal
export_stem_plot <- function(draw, path, graph_scale) {
  if (file.exists(path)) {
    file.remove(path)
  }
  open_png_device(path, width = 800 * graph_scale, height = 600 * graph_scale, units = "px", res = 90 * graph_scale)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw()
  invisible(NULL)
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
  if (is.null(draws) || ncol(draws) < 2) {
    cli::cli_abort("Unexpected posterior output from the hierarchical model.")
  }
  effect_draws <- draws[, 1]
  pub_bias_draws <- draws[, 2]
  effect_est <- mean(effect_draws)
  effect_se <- stats::sd(effect_draws)
  pub_bias_est <- mean(pub_bias_draws)
  pub_bias_se <- stats::sd(pub_bias_draws)
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
    n_model = nrow(data)
  )
}

run_selection <- function(df, total_n, options) {
  validate_columns(df, c("effect", "se"))
  data <- df[, c("effect", "se"), drop = FALSE]
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
    model = model
  )
  if (length(estimates$Psihat) < 2 || length(estimates$SE) < 2) {
    cli::cli_abort("Selection model did not return effect and publication bias parameters.")
  }
  effect_est <- estimates$Psihat[1]
  effect_se <- estimates$SE[1]
  pub_bias_est <- estimates$Psihat[2]
  pub_bias_se <- estimates$SE[2]
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
  if (length(estimates) < 4) {
    cli::cli_abort("Endogenous kink model did not return the expected coefficients.")
  }
  effect_est <- estimates[1]
  effect_se <- estimates[2]
  pub_bias_est <- estimates[3]
  pub_bias_se <- estimates[4]
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
    n_model = nrow(data)
  )
}

build_summary_table <- function(coefficients, digits) {
  if (!nrow(coefficients)) {
    return(data.frame())
  }
  row_labels <- c(
    "Publication Bias",
    "(Std. Error)",
    "Effect Beyond Bias",
    "(Std. Error)",
    "Total observations",
    "Model observations"
  )
  summary <- data.frame(Metric = row_labels, check.names = FALSE, stringsAsFactors = FALSE)
  for (model in unique(coefficients$model)) {
    model_rows <- coefficients[coefficients$model == model, , drop = FALSE]
    pb_row <- model_rows[model_rows$term == "publication_bias", , drop = FALSE]
    eff_row <- model_rows[model_rows$term == "effect", , drop = FALSE]
    total_obs <- unique(model_rows$n_obs_total)
    total_obs <- total_obs[is.finite(total_obs)]
    model_obs <- unique(model_rows$n_obs_model)
    model_obs <- model_obs[is.finite(model_obs)]
    column <- c(
      if (nrow(pb_row)) pb_row$estimate_formatted else "",
      if (nrow(pb_row)) pb_row$std_error_formatted else "",
      if (nrow(eff_row)) eff_row$estimate_formatted else "",
      if (nrow(eff_row)) eff_row$std_error_formatted else "",
      if (length(total_obs)) format_number(total_obs[1], 0) else "",
      if (length(model_obs)) format_number(model_obs[1], 0) else ""
    )
    column[is.na(column)] <- ""
    summary[[model_rows$model_label[1]]] <- column
  }
  attr(summary, "row.names") <- row_labels
  summary
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
          estimate = effect$estimate,
          std_error = effect$std_error,
          p_value = effect$p_value,
          n_obs_total = total_n,
          n_obs_model = n_model,
          stringsAsFactors = FALSE
        )
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
  run_nonlinear_methods
)

# nocov end -------------------------------------------------------------------
