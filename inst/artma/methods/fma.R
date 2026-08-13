#' @title Frequentist Model Averaging
#' @description
#' Run frequentist model averaging for heterogeneity analysis, using a BMA
#' model to order predictors. Reuses a provided BMA result when available.
fma <- function(df, bma_result = NULL) {
  box::use(
    artma / data_config / read[get_data_config],
    artma / econometric / bma[handle_bma_params, run_bma],
    artma / econometric / fma[run_fma],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[validate, validate_columns],
    artma / methods / bma[prepare_bma_inputs, unwrap_bma_result],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Frequentist Model Averaging")
  }

  config <- get_data_config()
  fma_opt <- get_option_group("artma.methods.fma")
  bma_opt <- get_option_group("artma.methods.bma")

  fma_resolved <- resolve_options(fma_opt, list(
    verbose_output = opt_spec(default = FALSE, type = "logical"),
    print_results = opt_spec(
      default = "fast", type = "character",
      constraint = function(x) x %in% c("none", "fast", "verbose", "all"),
      constraint_msg = "print_results must be one of: none, fast, verbose, all"
    ),
    round_to = opt_spec(default = NA_integer_, type = "numeric", allow_na = TRUE),
    cluster = opt_spec(default = TRUE, type = "logical")
  ))

  verbose_output <- fma_resolved$verbose_output
  print_results <- fma_resolved$print_results
  round_to <- fma_resolved$round_to
  cluster <- fma_resolved$cluster

  # Suppress individual FMA text output unless verbose_output is enabled
  effective_print <- if (verbose_output) print_results else "none"

  bma_resolved <- resolve_options(bma_opt, list(
    burn = opt_spec(default = 10000L, type = "numeric"),
    iter = opt_spec(default = 50000L, type = "numeric"),
    g = opt_spec(default = "UIP", type = "character"),
    mprior = opt_spec(default = "uniform", type = "character"),
    nmodel = opt_spec(default = 1000L, type = "numeric"),
    mcmc = opt_spec(default = "bd", type = "character"),
    use_vif_optimization = opt_spec(default = FALSE, type = "logical"),
    max_groups_to_remove = opt_spec(default = 30L, type = "numeric")
  ))

  burn <- bma_resolved$burn
  iter <- bma_resolved$iter
  g <- bma_resolved$g
  mprior <- bma_resolved$mprior
  nmodel <- bma_resolved$nmodel
  mcmc <- bma_resolved$mcmc
  use_vif_optimization <- bma_resolved$use_vif_optimization
  max_groups_to_remove <- bma_resolved$max_groups_to_remove

  bma_params_list <- list(
    burn = as.integer(burn),
    iter = as.integer(iter),
    g = g,
    mprior = mprior,
    nmodel = as.integer(nmodel),
    mcmc = mcmc
  )

  bma_params <- handle_bma_params(bma_params_list)
  if (length(bma_params) > 1 && get_verbosity() >= 2) {
    cli::cli_alert_warning("Multiple BMA parameter sets detected. Using the first set for FMA.")
  }
  bma_params <- bma_params[[1]]

  resolved <- unwrap_bma_result(bma_result)
  bma_model <- resolved$model
  bma_data <- resolved$data
  bma_var_list <- resolved$var_list

  if (is.null(bma_data) || is.null(bma_var_list)) {
    prepared <- prepare_bma_inputs(
      df = df,
      config = config,
      use_vif_optimization = use_vif_optimization,
      max_groups_to_remove = max_groups_to_remove,
      # Match the orchestrated path, which reuses the scaled frame from bma():
      # both paths must report coefficients on the same scale.
      scale_data = TRUE,
      verbosity = get_verbosity()
    )

    if (!is.null(prepared$skipped)) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("{prepared$skipped}. Skipping FMA analysis.")
      }
      empty_coefs <- data.frame(
        variable = character(0),
        coefficient = numeric(0),
        se = numeric(0),
        p_value = numeric(0),
        stringsAsFactors = FALSE
      )
      return(new_method_result(
        tables = list(coefficients = empty_coefs),
        meta = list(weights = numeric(0), model = NULL, skip_reason = prepared$skipped)
      ))
    }

    bma_data <- prepared$bma_data
    bma_var_list <- prepared$bma_var_list
  }

  if (is.null(bma_model)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Running BMA to obtain predictor ordering for FMA...")
    }
    bma_model <- run_bma(bma_data, bma_params, quiet = !verbose_output)
  }

  cluster_ids <- if (cluster) resolve_fma_cluster_ids(df, bma_data) else NULL

  fma_results <- run_fma(
    bma_data = bma_data,
    bma_model = bma_model,
    input_var_list = bma_var_list,
    cluster_ids = cluster_ids,
    round_to = round_to,
    print_results = effective_print
  )

  new_method_result(
    tables = list(coefficients = fma_results$coefficients),
    estimates = fma_estimates(
      fma_results$coefficients,
      n_obs = nrow(bma_data),
      n_clusters = if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids))
    ),
    meta = list(
      weights = fma_results$weights,
      model = bma_model,
      data = bma_data,
      params = bma_params,
      var_list = bma_var_list,
      clustered = !is.null(cluster_ids)
    )
  )
}

#' @title Tidy estimates for FMA
#' @description
#' Map the model-averaged coefficient frame onto the shared `estimates` schema,
#' one row per moderator. The averaged coefficient, its standard error, and the
#' Wald p-value carry over unrounded; `n_clusters` is filled only when the
#' standard errors are clustered, so an `NA` there means iid inference.
#' @param coefficients *\[data.frame\]* The FMA coefficient frame
#'   (`variable`, `coefficient`, `se`, `p_value`).
#' @param n_obs *\[integer, optional\]* Observations the FMA was fitted on.
#' @param n_clusters *\[integer, optional\]* Number of clusters behind the
#'   standard errors, or `NA` when they are not clustered.
#' @return *\[data.frame\]* A frame in the shared estimates schema.
fma_estimates <- function(coefficients, n_obs = NA_integer_, n_clusters = NA_integer_) {
  box::use(
    artma / modules / runtime_methods[new_estimates]
  )

  if (!is.data.frame(coefficients) || nrow(coefficients) == 0L) {
    return(new_estimates())
  }

  new_estimates(data.frame(
    method = "fma",
    term = coefficients$variable,
    estimate = coefficients$coefficient,
    std_error = coefficients$se,
    p_value = coefficients$p_value,
    n_obs = n_obs,
    n_clusters = n_clusters,
    stringsAsFactors = FALSE
  ))
}

#' Align `study_id` cluster IDs with the prepared BMA frame
#'
#' `prepare_bma_inputs()` drops rows via `stats::na.omit()`, so the prepared
#' frame is matched back to `df` through its surviving row names. Returns
#' `NULL` (iid standard errors) with a warning whenever a usable cluster
#' vector cannot be built.
#'
#' @param df *\[data.frame\]* The preprocessed data frame passed to the method.
#' @param bma_data *\[data.frame\]* The prepared BMA/FMA estimation frame.
#' @return *\[vector, optional\]* Cluster IDs aligned with `bma_data` rows, or `NULL`.
resolve_fma_cluster_ids <- function(df, bma_data) {
  box::use(
    artma / libs / core / utils[get_verbosity]
  )

  # Downgrading to iid inference changes reported SEs materially; never do it
  # silently (mirrors robust_vcov's warn_downgrade contract).
  warn_downgrade <- function(reason) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "Clustered FMA unavailable ({reason}); standard errors are NOT clustered."
      )
    }
  }

  if (!"study_id" %in% colnames(df)) {
    warn_downgrade("the data have no study_id column")
    return(NULL)
  }

  ids <- df[rownames(bma_data), "study_id"]
  if (any(is.na(ids))) {
    warn_downgrade("cluster IDs could not be aligned with the estimation frame")
    return(NULL)
  }
  if (length(unique(ids)) < 2L) {
    warn_downgrade("fewer than two distinct clusters")
    return(NULL)
  }

  ids
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  fma,
  stage = "fma",
  depends_on = "bma",
  required_columns = c("effect", "se"),
  suggests = c("BMS", "quadprog")
)

box::export(fma, fma_estimates, resolve_fma_cluster_ids, run)
