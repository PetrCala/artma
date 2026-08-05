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
    artma / libs / core / validation[validate, validate_columns],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Robust Bayesian Meta-Analysis")
  }

  opt <- get_option_group("artma.methods.robma")

  resolved <- resolve_options(opt, list(
    chains = opt_spec(
      default = 3L, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "chains must be positive"
    ),
    samples = opt_spec(
      default = 5000L, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "samples must be positive"
    ),
    burnin = opt_spec(
      default = 2000L, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "burnin must be positive"
    ),
    adapt = opt_spec(
      default = 500L, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "adapt must be positive"
    ),
    autofit = opt_spec(default = TRUE, type = "logical"),
    measure = opt_spec(
      default = "SMD", type = "character",
      constraint = function(x) x %in% c("SMD", "ZCOR", "RR", "OR", "HR", "RD", "IRR", "GEN"),
      constraint_msg = "measure must be one of: SMD, ZCOR, RR, OR, HR, RD, IRR, GEN"
    ),
    effect_prior_scale = opt_spec(
      default = 0.707, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "effect_prior_scale must be positive"
    ),
    heterogeneity_shape = opt_spec(
      default = 1, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "heterogeneity_shape must be positive"
    ),
    heterogeneity_scale = opt_spec(
      default = 0.15, type = "numeric",
      constraint = function(x) x > 0, constraint_msg = "heterogeneity_scale must be positive"
    ),
    round_to = opt_spec(
      default = 3L, type = "numeric", key = "artma.output.number_of_decimals"
    )
  ))

  chains <- resolved$chains
  samples <- resolved$samples
  burnin <- resolved$burnin
  adapt <- resolved$adapt
  autofit <- resolved$autofit
  measure <- resolved$measure
  effect_prior_scale <- resolved$effect_prior_scale
  heterogeneity_shape <- resolved$heterogeneity_shape
  heterogeneity_scale <- resolved$heterogeneity_scale
  round_to <- resolved$round_to

  # parallel is auto-detected from a possibly-NA option and seed permits NA, so
  # both stay outside the spec-driven block.
  parallel <- resolve_parallel(opt$parallel, chains)
  seed <- opt$seed %||% NA_integer_
  validate(is.numeric(seed) || is.na(seed))

  usable <- is.finite(df$effect) & is.finite(df$se) & df$se > 0
  fit_data <- df[usable, , drop = FALSE]

  # RoBMA's likelihood treats rows as independent; meta-analysis data carries
  # several estimates per study, so pass study ids for multilevel clustering.
  cluster_ids <- if ("study_id" %in% colnames(fit_data)) fit_data$study_id else NULL
  if (is.null(cluster_ids) && get_verbosity() >= 2) {
    cli::cli_alert_warning(
      "No study_id column: RoBMA treats all estimates as independent."
    )
  }

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
    yi = fit_data$effect,
    sei = fit_data$se,
    cluster = cluster_ids,
    measure = measure,
    prior_effect = effect_prior,
    prior_heterogeneity = heterogeneity_prior,
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
  models <- RoBMA::summary_models(fit, type = "individual")

  estimates <- as_robma_table(ensemble$estimates, "parameter", round_to)
  components <- as_robma_table(ensemble$inclusion_components, "component", round_to)
  model_table <- as_robma_table(models$individual, "model", round_to)

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

#' @title Resolve the RoBMA parallel sampling flag
#' @description
#' RoBMA spawns one worker per chain when `parallel = TRUE`, so parallel
#' sampling only pays off when the machine has a spare core on top of the
#' chains. Auto-detect that when the user left the option unset (`NA`); an
#' explicit `TRUE`/`FALSE` in the options file always wins.
#' @param setting *\[logical, optional\]* The `parallel` option value.
#' @param chains *\[numeric\]* Number of MCMC chains to be fitted.
#' @return *\[logical\]* Whether to sample the chains in parallel.
resolve_parallel <- function(setting, chains) {
  if (!is.null(setting) && !is.na(setting)) {
    return(isTRUE(setting))
  }

  if (tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")) %in% c("true", "warn")) {
    return(FALSE)
  }

  cores <- parallel::detectCores()
  if (is.na(cores)) {
    cores <- 1L
  }

  cores > chains
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
  opt_in = TRUE,
  cached = TRUE
)

box::export(robma, run)
