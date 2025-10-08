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
  variables <- opt$variables

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

  # Get BMA variables - prompt user if not configured
  # Priority: 1) variables from options, 2) legacy bma=TRUE in config, 3) prompt user
  if (!is.null(variables) && length(variables) > 0 && !all(is.na(variables))) {
    bma_vars <- variables
  } else {
    # Fall back to legacy method (checking bma=TRUE in config)
    bma_vars <- names(config)[vapply(config, function(var_cfg) {
      if (!is.list(var_cfg)) {
        return(FALSE)
      }
      isTRUE(var_cfg$bma)
    }, logical(1))]

    # If still no variables, prompt the user
    if (!length(bma_vars)) {
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("No BMA variables configured. Please select variables for analysis.")
      }

      # Prompt for variable selection
      bma_vars <- prompt_bma_variable_selection(df, config)

      # Save the selected variables to options for future runs
      if (length(bma_vars) > 0) {
        save_bma_variables_to_options(bma_vars)
      }
    }
  }

  missing_vars <- bma_vars[!bma_vars %in% names(df)]
  if (length(missing_vars)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Missing BMA variables in data: {.val {missing_vars}}")
    }
    bma_vars <- bma_vars[bma_vars %in% names(df)]
  }

  if (!length(bma_vars)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No valid BMA variables available. Skipping BMA analysis.")
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
      skipped = "No valid BMA variables available"
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

#' Prompt user to select BMA variables at runtime
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data config
#' @return *\[character\]* Selected variable names
prompt_bma_variable_selection <- function(df, config) {
  box::use(artma / libs / utils[get_verbosity])

  # First, prompt for selection mode
  selection_mode <- prompt_bma_variable_selection_mode()

  selection <- if (selection_mode == "auto") auto_select_bma_variables(df, config) else manual_select_bma_variables(df, config)

  selection
}

#' Prompt for manual or auto variable selection mode
#' @return *\[character\]* Either "manual" or "auto"
prompt_bma_variable_selection_mode <- function() {
  box::use(artma / const[CONST])

  choices <- c(
    "Manual selection (interactive menu)" = "manual",
    "Automatic detection (not yet implemented)" = "auto"
  )

  cli::cli_h1("BMA Variable Selection Mode")
  cli::cli_text("Choose how to select variables for Bayesian Model Averaging analysis.")
  cli::cat_line()

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Select variable selection mode",
    selected = 1 # "manual"
  )

  if (rlang::is_empty(selected)) {
    cli::cli_alert_info("No selection made. Using default: {CONST$STYLES$OPTIONS$VALUE('manual')}")
    return("manual")
  }

  selected_value <- choices[selected][[1]]
  cli::cli_alert_success("Selected mode: {CONST$STYLES$OPTIONS$VALUE(selected_value)}")
  cli::cat_line()

  selected_value
}

#' Automatically select BMA variables (placeholder)
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data config
#' @return *\[character\]* Selected variable names
auto_select_bma_variables <- function(df, config) {
  box::use(artma / libs / utils[get_verbosity])

  # Placeholder for automatic variable selection
  # This will be implemented later with logic to automatically detect
  # suitable variables for BMA analysis
  if (get_verbosity() >= 2) {
    cli::cli_alert_warning("Automatic variable selection is not yet implemented.")
    cli::cli_alert_info("Falling back to manual selection...")
  }

  manual_select_bma_variables(df, config)
}

#' Manual variable selection via interactive menu
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data config
#' @return *\[character\]* Selected variable names
manual_select_bma_variables <- function(df, config) {
  box::use(
    artma / const[CONST],
    artma / libs / utils[get_verbosity]
  )

  cli::cli_h1("BMA Variable Selection")
  cli::cat_line()

  if (is.null(config) || length(config) == 0) {
    cli::cli_alert_warning("No variables available for BMA analysis.")
    return(character(0))
  }

  # Get potential moderator variables (exclude effect, se, and other special columns)
  excluded_vars <- c("effect", "se", "obs_id", "study", "study_id", "t_stat", "precision", "reg_dof", "n_obs", "study_size")

  potential_vars <- names(config)[!names(config) %in% excluded_vars]

  # Filter to only variables that exist in the data
  if (!is.null(df)) {
    potential_vars <- potential_vars[potential_vars %in% names(df)]
  }

  if (length(potential_vars) == 0) {
    cli::cli_alert_warning("No moderator variables available for BMA analysis.")
    return(character(0))
  }

  # Create display names for variables
  var_display_names <- vapply(potential_vars, function(var) {
    var_config <- config[[var]]
    if (!is.null(var_config$var_name_verbose)) {
      paste0(var, " (", var_config$var_name_verbose, ")")
    } else {
      var
    }
  }, character(1))

  # Display instructions
  cli::cli_h2("Instructions")
  cli::cli_text("{cli::symbol$info} The {.strong effect} variable is automatically used as the dependent variable")
  cli::cli_text("{cli::symbol$info} Select moderator variables to include in the BMA analysis")
  cli::cli_alert_warning("{cli::symbol$warning} {.strong Important:} Ensure you avoid the dummy variable trap!")
  cli::cli_ul(c(
    "Do not select all categories of a categorical variable (omit one as reference)",
    "Avoid selecting variables with perfect collinearity",
    "Be cautious with highly correlated variables (consider using VIF optimization)"
  ))
  cli::cat_line()

  # Multi-select menu
  selected_indices <- climenu::checkbox(
    choices = var_display_names,
    prompt = "Select variables to include in BMA analysis (use SPACE to select, ENTER to confirm)",
    return_index = TRUE
  )

  if (rlang::is_empty(selected_indices) || length(selected_indices) == 0) {
    cli::cli_alert_warning("No variables selected. BMA analysis will be skipped.")
    return(character(0))
  }

  selected_vars <- potential_vars[selected_indices]

  cli::cli_alert_success("Selected {length(selected_vars)} variable{?s} for BMA analysis:")
  cli::cli_ul(selected_vars)
  cli::cat_line()

  selected_vars
}

#' Save BMA variables to options
#' @param variables *\[character\]* Variable names to save
save_bma_variables_to_options <- function(variables) {
  box::use(artma / libs / utils[get_verbosity])

  tryCatch(
    {
      # Update in-memory options
      options(artma.methods.bma.variables = variables)

      # Try to persist to file if we have an options file loaded
      options_file <- getOption("artma.temp.file_name")
      if (!is.null(options_file)) {
        artma::options.modify(
          options_file_name = options_file,
          user_input = list(methods = list(bma = list(variables = variables))),
          should_validate = FALSE
        )
        if (get_verbosity() >= 3) {
          cli::cli_alert_success("BMA variables saved to options file for future runs")
        }
      } else {
        if (get_verbosity() >= 3) {
          cli::cli_alert_success("BMA variables saved to current session (no options file to persist to)")
        }
      }
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Could not save BMA variables to options file: {e$message}")
        cli::cli_alert_info("Variables will be used for this session only")
      }
      # Still update in-memory options even if file save fails
      options(artma.methods.bma.variables = variables)
    }
  )
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
