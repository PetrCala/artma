#' @title Non-linear model helpers
#' @description Helper utilities to run publication-bias corrections based on
#'   non-linear estimators.
NULL

box::use(
  stats[quantile],
  artma / libs / validation[validate, validate_columns, assert],
  artma / libs / result_formatters[
    significance_mark,
    format_number,
    format_se
  ],
  artma / calc / methods / stem[stem],
  artma / calc / methods / selection_model[metastudies_estimation],
  artma / calc / methods / endo_kink[run_endogenous_kink]
)

# nocov start -----------------------------------------------------------------

normal_p_value <- function(estimate, std_error) {
  if (!is.finite(estimate) || !is.finite(std_error) || std_error <= 0) {
    return(NA_real_)
  }
  2 * stats::pnorm(abs(estimate / std_error), lower.tail = FALSE)
}

format_estimate <- function(estimate, p_value, digits, add_marks) {
  formatted <- format_number(estimate, digits)
  if (!length(formatted)) {
    return(character(0))
  }
  marks <- if (isTRUE(add_marks)) significance_mark(p_value) else ""
  formatted <- paste0(formatted, marks)
  formatted[is.na(formatted)] <- ""
  formatted
}

format_standard_error <- function(std_error, digits) {
  formatted <- format_se(std_error, digits)
  formatted[is.na(formatted)] <- ""
  formatted
}

nonlinear_method_specs <- function(options) {
  list(
    list(
      name = "waap",
      label = "WAAP",
      runner = function(df, total_n) run_waap(df, total_n)
    ),
    list(
      name = "top10",
      label = "Top10",
      runner = function(df, total_n) run_top10(df, total_n)
    ),
    list(
      name = "stem",
      label = "Stem",
      runner = function(df, total_n) run_stem(df, total_n, options)
    ),
    list(
      name = "hierarchical",
      label = "Hierarch",
      runner = function(df, total_n) run_hierarchical(df, total_n, options)
    ),
    list(
      name = "selection",
      label = "Selection",
      runner = function(df, total_n) run_selection(df, total_n, options)
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
    n_model = nrow(filtered)
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
    n_model = nrow(filtered)
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
    n_model = n_included
  )
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
  fit <- suppressWarnings(
    bayesm::rhierLinearModel(
      Data = list(regdata = regdata),
      Mcmc = list(R = iterations)
    )
  )
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
    n_model = nrow(data)
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

run_nonlinear_methods <- function(df, options) { # nolint: function_length_linter.
  validate(is.data.frame(df))
  total_n <- nrow(df)
  specs <- nonlinear_method_specs(options)
  results <- list()
  skipped <- list()
  for (spec in specs) {
    tryCatch(
      {
        method_result <- spec$runner(df, total_n)
        coefficients <- list()
        pb <- method_result$publication_bias %||% list(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_)
        effect <- method_result$effect
        n_model <- method_result$n_model %||% total_n
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
    return(list(coefficients = empty, summary = empty, skipped = skipped, options = options))
  }
  coefficients <- do.call(rbind, results)
  digits <- options$round_to %||% 3L
  add_marks <- isTRUE(options$add_significance_marks)
  coefficients$estimate_formatted <- format_estimate(coefficients$estimate, coefficients$p_value, digits, add_marks)
  coefficients$std_error_formatted <- format_standard_error(coefficients$std_error, digits)
  summary <- build_summary_table(coefficients, digits)
  list(
    coefficients = coefficients,
    summary = summary,
    skipped = skipped,
    options = options
  )
}

box::export(
  run_nonlinear_methods
)

# nocov end -------------------------------------------------------------------
