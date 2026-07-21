# Minimum number of usable observations required to attempt a fit.
MIN_OBSERVATIONS <- 3L

#' @title Robust Bayesian Meta-Analysis
#' @description
#' Run robust Bayesian meta-analysis (RoBMA, Bartos et al.) over the effect
#' estimates and their standard errors. The ensemble combines models with and
#' without an effect, heterogeneity, and publication bias, and reports both the
#' component inclusion probabilities and the model-averaged estimates.
#'
#' The prior structure follows the thesis specification: a Cauchy effect prior
#' truncated to (0, Inf) and an inverse-gamma heterogeneity prior. Only the
#' prior scale/shape values and the sampler settings are configurable.
#'
#' The method is computationally expensive and is registered as opt-in, so it
#' runs only when requested by name.
robma <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Robust Bayesian Meta-Analysis")
  }

  opt <- get_option_group("artma.methods.robma")

  chains <- opt$chains %||% 3L
  samples <- opt$samples %||% 5000L
  burnin <- opt$burnin %||% 2000L
  adapt <- opt$adapt %||% 500L
  parallel <- opt$parallel %||% FALSE
  autofit <- opt$autofit %||% TRUE
  seed <- opt$seed %||% NA_integer_
  effect_prior_scale <- opt$effect_prior_scale %||% 0.707
  heterogeneity_shape <- opt$heterogeneity_shape %||% 1
  heterogeneity_scale <- opt$heterogeneity_scale %||% 0.15
  round_to <- getOption("artma.output.number_of_decimals", 3)

  validate(
    is.numeric(chains),
    is.numeric(samples),
    is.numeric(burnin),
    is.numeric(adapt),
    is.logical(parallel),
    is.logical(autofit),
    is.numeric(seed) || is.na(seed),
    is.numeric(effect_prior_scale),
    is.numeric(heterogeneity_shape),
    is.numeric(heterogeneity_scale)
  )

  assert(chains > 0, "chains must be positive")
  assert(samples > 0, "samples must be positive")
  assert(burnin > 0, "burnin must be positive")
  assert(adapt > 0, "adapt must be positive")
  assert(effect_prior_scale > 0, "effect_prior_scale must be positive")
  assert(heterogeneity_shape > 0, "heterogeneity_shape must be positive")
  assert(heterogeneity_scale > 0, "heterogeneity_scale must be positive")

  usable <- is.finite(df$effect) & is.finite(df$se) & df$se > 0
  fit_data <- df[usable, , drop = FALSE]

  if (nrow(fit_data) < MIN_OBSERVATIONS) {
    reason <- sprintf(
      "fewer than %d usable observations (%d available)",
      MIN_OBSERVATIONS, nrow(fit_data)
    )
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Skipping RoBMA: {reason}")
    }
    return(new_method_result(
      tables = list(summary = empty_robma_table()),
      meta = list(model = NULL, n_obs = nrow(fit_data), skipped = reason)
    ))
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info(
      "Fitting RoBMA on {nrow(fit_data)} observations. This may take a while..."
    )
  }

  effect_prior <- RoBMA::prior(
    distribution = "cauchy",
    parameters = list(location = 0, scale = effect_prior_scale),
    truncation = list(lower = 0, upper = Inf)
  )
  heterogeneity_prior <- RoBMA::prior(
    distribution = "invgamma",
    parameters = list(shape = heterogeneity_shape, scale = heterogeneity_scale)
  )

  fit <- RoBMA::RoBMA(
    d = fit_data$effect,
    se = fit_data$se,
    priors_effect = effect_prior,
    priors_heterogeneity = heterogeneity_prior,
    chains = as.integer(chains),
    sample = as.integer(samples),
    burnin = as.integer(burnin),
    adapt = as.integer(adapt),
    parallel = isTRUE(parallel),
    autofit = isTRUE(autofit),
    seed = if (is.na(seed)) NULL else as.integer(seed),
    silent = get_verbosity() < 4
  )

  ensemble <- summary(fit)
  models <- summary(fit, type = "models")

  estimates <- as_robma_table(ensemble$estimates, "parameter", round_to)
  components <- as_robma_table(ensemble$components, "component", round_to)
  model_table <- as_robma_table(models$summary, "model", round_to)

  if (get_verbosity() >= 3) {
    cli::cli_h3("RoBMA: model-averaged estimates")
    print(estimates) # nolint: undesirable_function_linter.
  }

  new_method_result(
    tables = list(
      summary = estimates,
      components = components,
      models = model_table
    ),
    meta = list(model = fit, n_obs = nrow(fit_data))
  )
}

#' @title Empty RoBMA estimates table
#' @description Column-compatible placeholder returned when the method is
#'   skipped for lack of usable observations.
#' @return *\[data.frame\]* A zero-row estimates table.
empty_robma_table <- function() {
  data.frame(
    parameter = character(0),
    Mean = numeric(0),
    Median = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' @title Coerce a RoBMA summary table to a plain data frame
#' @description
#' RoBMA (via BayesTools) returns tables carrying extra classes and attributes
#' and holding the parameter labels in the row names. Flatten one into a plain
#' `data.frame` with the labels in a leading column and numeric columns rounded.
#' @param tbl *\[data.frame\]* A table from `summary.RoBMA`.
#' @param label_column *\[character\]* Name of the leading label column.
#' @param round_to *\[numeric\]* Number of decimals for numeric columns.
#' @return *\[data.frame\]* A plain data frame.
as_robma_table <- function(tbl, label_column, round_to) {
  if (is.null(tbl) || nrow(tbl) == 0L) {
    return(data.frame(stats::setNames(list(character(0)), label_column)))
  }

  labels <- rownames(tbl)
  out <- as.data.frame(unclass(tbl), stringsAsFactors = FALSE)
  rownames(out) <- NULL

  if (is.numeric(round_to) && !is.na(round_to)) {
    numeric_cols <- vapply(out, is.numeric, logical(1))
    out[numeric_cols] <- lapply(out[numeric_cols], round, digits = round_to)
  }

  out <- cbind(
    stats::setNames(
      data.frame(labels %||% seq_len(nrow(out)), stringsAsFactors = FALSE),
      label_column
    ),
    out
  )

  out
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  robma,
  stage = "robma",
  required_columns = c("effect", "se"),
  suggests = "RoBMA",
  opt_in = TRUE
)

box::export(robma, run)
