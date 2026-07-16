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
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / methods / bma[prepare_bma_inputs],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Frequentist Model Averaging")
  }

  config <- get_data_config()
  fma_opt <- get_option_group("artma.methods.fma")
  bma_opt <- get_option_group("artma.methods.bma")

  verbose_output <- fma_opt$verbose_output %||% FALSE
  print_results <- fma_opt$print_results %||% "fast"
  round_to <- fma_opt$round_to %||% NA_integer_

  validate(
    is.logical(verbose_output),
    is.character(print_results),
    is.numeric(round_to) || is.na(round_to)
  )

  assert(
    print_results %in% c("none", "fast", "verbose", "all"),
    "print_results must be one of: none, fast, verbose, all"
  )

  # Suppress individual FMA text output unless verbose_output is enabled
  effective_print <- if (verbose_output) print_results else "none"

  burn <- bma_opt$burn %||% 10000L
  iter <- bma_opt$iter %||% 50000L
  g <- bma_opt$g %||% "UIP"
  mprior <- bma_opt$mprior %||% "uniform"
  nmodel <- bma_opt$nmodel %||% 1000L
  mcmc <- bma_opt$mcmc %||% "bd"
  use_vif_optimization <- bma_opt$use_vif_optimization %||% FALSE
  max_groups_to_remove <- bma_opt$max_groups_to_remove %||% 30L

  validate(
    is.numeric(burn),
    is.numeric(iter),
    is.character(g),
    is.character(mprior),
    is.numeric(nmodel),
    is.character(mcmc),
    is.logical(use_vif_optimization),
    is.numeric(max_groups_to_remove)
  )

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

  resolve_bma_result <- function(result) {
    if (!is.list(result)) {
      return(list())
    }
    # Accept the standard method contract (fields under meta) or a bare
    # meta-like list for callers that pass one directly.
    meta <- result$meta %||% result
    if (!is.null(meta$model) || !is.null(meta$data) || !is.null(meta$var_list)) {
      return(list(
        model = meta$model,
        data = meta$data,
        var_list = meta$var_list
      ))
    }
    list()
  }

  resolved <- resolve_bma_result(bma_result)
  bma_model <- resolved$model
  bma_data <- resolved$data
  bma_var_list <- resolved$var_list

  if (is.null(bma_data) || is.null(bma_var_list)) {
    prepared <- prepare_bma_inputs(
      df = df,
      config = config,
      use_vif_optimization = use_vif_optimization,
      max_groups_to_remove = max_groups_to_remove,
      scale_data = FALSE,
      verbosity = get_verbosity()
    )

    if (!is.null(prepared$skipped)) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("No valid BMA variables available. Skipping FMA analysis.")
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
        meta = list(weights = numeric(0), model = NULL, skipped = prepared$skipped)
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

  fma_results <- run_fma(
    bma_data = bma_data,
    bma_model = bma_model,
    input_var_list = bma_var_list,
    round_to = round_to,
    print_results = effective_print
  )

  new_method_result(
    tables = list(coefficients = fma_results$coefficients),
    meta = list(
      weights = fma_results$weights,
      model = bma_model,
      data = bma_data,
      params = bma_params,
      var_list = bma_var_list
    )
  )
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(fma, stage = "fma")

box::export(fma, run)
