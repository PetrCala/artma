#' @title Bayesian Model Averaging
#' @description
#' Perform Bayesian Model Averaging to examine heterogeneity in meta-analysis
#' effects across moderator variables. The method uses the BMS package to
#' estimate the posterior inclusion probability (PIP) and coefficient estimates
#' for each potential moderator variable.
bma <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data_config / read[get_data_config],
    artma / libs / bma[
      get_bma_data,
      get_bma_formula,
      handle_bma_params,
      run_bma,
      extract_bma_results,
      find_optimal_bma_formula
    ],
    artma / libs / utils[get_verbosity],
    artma / libs / validation[assert, validate, validate_columns],
    artma / options / index[get_option_group]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "se"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Bayesian Model Averaging")
  }

  config <- get_data_config()
  opt <- get_option_group("artma.methods.bma")

  burn <- opt$burn %||% 10000L
  iter <- opt$iter %||% 50000L
  g <- opt$g %||% "UIP"
  mprior <- opt$mprior %||% "uniform"
  nmodel <- opt$nmodel %||% 1000L
  mcmc <- opt$mcmc %||% "bd"
  use_vif_optimization <- opt$use_vif_optimization %||% FALSE
  max_groups_to_remove <- opt$max_groups_to_remove %||% 30L
  print_results <- opt$print_results %||% "fast"
  export_graphics <- opt$export_graphics %||% FALSE
  export_path <- opt$export_path %||% "./results/graphic"
  graph_scale <- opt$graph_scale %||% 1

  validate(
    is.numeric(burn),
    is.numeric(iter),
    is.character(g),
    is.character(mprior),
    is.numeric(nmodel),
    is.character(mcmc),
    is.logical(use_vif_optimization),
    is.numeric(max_groups_to_remove),
    is.character(print_results),
    is.logical(export_graphics),
    is.character(export_path),
    is.numeric(graph_scale)
  )

  assert(burn > 0, "burn must be positive")
  assert(iter > 0, "iter must be positive")
  assert(nmodel > 0, "nmodel must be positive")
  assert(max_groups_to_remove > 0, "max_groups_to_remove must be positive")
  assert(graph_scale > 0, "graph_scale must be positive")
  assert(
    print_results %in% c("none", "fast", "verbose", "all", "table"),
    "print_results must be one of: none, fast, verbose, all, table"
  )

  bma_vars <- names(config)[vapply(config, function(var_cfg) {
    if (!is.list(var_cfg)) {
      return(FALSE)
    }
    isTRUE(var_cfg$bma)
  }, logical(1))]

  if (!length(bma_vars)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No variables selected for BMA analysis")
    }
    return(list(
      coefficients = data.frame(
        variable = character(0),
        pip = numeric(0),
        post_mean = numeric(0),
        post_sd = numeric(0),
        cond_pos_sign = numeric(0),
        stringsAsFactors = FALSE
      ),
      model = NULL,
      skipped = "No BMA variables configured"
    ))
  }

  missing_vars <- bma_vars[!bma_vars %in% names(df)]
  if (length(missing_vars)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Missing BMA variables in data: {.val {missing_vars}}")
    }
    bma_vars <- bma_vars[bma_vars %in% names(df)]
  }

  if (!length(bma_vars)) {
    return(list(
      coefficients = data.frame(
        variable = character(0),
        pip = numeric(0),
        post_mean = numeric(0),
        post_sd = numeric(0),
        cond_pos_sign = numeric(0),
        stringsAsFactors = FALSE
      ),
      model = NULL,
      skipped = "No valid BMA variables available in data"
    ))
  }

  bma_var_list <- data.frame(
    var_name = c("effect", "se", bma_vars),
    var_name_verbose = vapply(c("effect", "se", bma_vars), function(v) {
      if (v %in% names(config) && !is.null(config[[v]]$var_name_verbose)) {
        config[[v]]$var_name_verbose
      } else {
        v
      }
    }, character(1)),
    bma = c(TRUE, TRUE, rep(TRUE, length(bma_vars))),
    group_category = vapply(c("effect", "se", bma_vars), function(v) {
      if (v %in% names(config) && !is.null(config[[v]]$group_category)) {
        config[[v]]$group_category
      } else {
        "other"
      }
    }, character(1)),
    to_log_for_bma = vapply(c("effect", "se", bma_vars), function(v) {
      if (v %in% names(config) && !is.null(config[[v]]$bma_to_log)) {
        config[[v]]$bma_to_log
      } else {
        FALSE
      }
    }, logical(1)),
    bma_reference_var = rep(FALSE, length(c("effect", "se", bma_vars))),
    stringsAsFactors = FALSE
  )

  if (use_vif_optimization) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Searching for optimal BMA formula using VIF optimization...")
    }
    formula_result <- find_optimal_bma_formula(
      df,
      bma_var_list,
      max_groups_to_remove = max_groups_to_remove,
      return_variable_vector_instead = FALSE,
      verbose = get_verbosity() >= 3
    )
    bma_formula <- formula_result$formula
    bma_var_names <- all.vars(bma_formula)
  } else {
    bma_var_names <- c("effect", "se", bma_vars)
  }

  bma_data <- get_bma_data(
    df,
    bma_var_list,
    variable_info = bma_var_names,
    scale_data = TRUE,
    from_vector = TRUE,
    include_reference_groups = FALSE
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

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Running BMA with {nrow(bma_data)} observations and {ncol(bma_data) - 1} potential moderators...")
  }

  results <- list()
  for (i in seq_along(bma_params)) {
    if (get_verbosity() >= 3 && length(bma_params) > 1) {
      cli::cli_alert_info("Running BMA model {i} of {length(bma_params)}...")
    }

    bma_model <- run_bma(bma_data, bma_params[[i]])

    bma_coefs <- extract_bma_results(
      bma_model,
      bma_data,
      bma_var_list,
      print_results = print_results,
      adjustable_theme = FALSE,
      theme = "blue",
      export_graphics = export_graphics,
      export_path = export_path,
      graph_scale = graph_scale
    )

    pip_values <- BMS::pmp.bma(bma_model)

    coef_df <- data.frame(
      variable = names(bma_coefs),
      pip = pip_values[match(names(bma_coefs), names(pip_values))],
      post_mean = as.numeric(bma_coefs),
      post_sd = as.numeric(NA),
      cond_pos_sign = as.numeric(NA),
      stringsAsFactors = FALSE
    )

    rownames(coef_df) <- NULL

    results[[i]] <- list(
      coefficients = coef_df,
      model = bma_model,
      data = bma_data,
      params = bma_params[[i]]
    )
  }

  if (length(results) == 1) {
    if (get_verbosity() >= 3) {
      cli::cli_h3("BMA Coefficients")
      cli::cat_print(results[[1]]$coefficients)
    }
    results[[1]]
  } else {
    if (get_verbosity() >= 3) {
      cli::cli_h3("BMA Results (multiple models)")
      for (i in seq_along(results)) {
        cli::cli_alert_info("Model {i}:")
        cli::cat_print(results[[i]]$coefficients)
      }
    }
    results
  }
}


box::use(
  artma / libs / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  bma,
  stage = "bma",
  key_builder = function(...) build_data_cache_signature()
)

box::export(bma, run)
