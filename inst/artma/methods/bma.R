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
    artma / econometric / bma[
      get_bma_data,
      get_bma_formula,
      handle_bma_params,
      run_bma,
      extract_bma_results,
      find_optimal_bma_formula
    ],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
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

  # Get BMA variables from data config (bma=TRUE)
  bma_vars <- names(config)[vapply(config, function(var_cfg) {
    if (!is.list(var_cfg)) {
      return(FALSE)
    }
    isTRUE(var_cfg$bma)
  }, logical(1))]

  # If no variables configured, prompt the user
  if (!length(bma_vars)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No BMA variables configured. Please select variables for analysis.")
    }

    # Prompt for variable selection
    bma_vars <- prompt_bma_variable_selection(df, config)

    # Save the selected variables to data config for future runs
    if (length(bma_vars) > 0) {
      save_bma_variables_to_data_config(bma_vars, config)
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

  # Only include 'effect' as required (dependent variable)
  # All other variables (including 'se') must be explicitly selected by the user
  all_vars <- c("effect", bma_vars)

  bma_var_list <- data.frame(
    var_name = all_vars,
    var_name_verbose = vapply(all_vars, function(v) {
      if (v %in% names(config) && !is.null(config[[v]]$var_name_verbose) && is.character(config[[v]]$var_name_verbose)) {
        config[[v]]$var_name_verbose
      } else {
        v
      }
    }, character(1)),
    bma = rep(TRUE, length(all_vars)),
    group_category = vapply(all_vars, function(v) {
      if (v %in% names(config) && !is.null(config[[v]]$group_category) && is.character(config[[v]]$group_category)) {
        config[[v]]$group_category
      } else {
        "other"
      }
    }, character(1)),
    to_log_for_bma = vapply(all_vars, function(v) {
      if (v %in% names(config) && !is.null(config[[v]]$bma_to_log) && is.logical(config[[v]]$bma_to_log)) {
        config[[v]]$bma_to_log
      } else {
        FALSE
      }
    }, logical(1)),
    bma_reference_var = rep(FALSE, length(all_vars)),
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
    bma_var_names <- all_vars
  }

  bma_data <- get_bma_data(
    df,
    bma_var_list,
    variable_info = bma_var_names,
    scale_data = TRUE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  # Check for and handle missing values
  if (any(is.na(bma_data))) {
    na_count <- sum(is.na(bma_data))
    row_count_before <- nrow(bma_data)

    # Remove rows with any NA values
    bma_data <- stats::na.omit(bma_data)

    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Removed {row_count_before - nrow(bma_data)} observation{?s} with missing values ({na_count} NA value{?s} total)")
    }

    if (nrow(bma_data) == 0) {
      cli::cli_abort("No observations remaining after removing missing values. Cannot run BMA.")
    }
  }

  # Check for constant variables and remove them
  is_constant <- vapply(bma_data, function(x) length(unique(x)) <= 1, logical(1))
  if (any(is_constant)) {
    constant_vars <- names(bma_data)[is_constant]
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Removing {length(constant_vars)} constant variable{?s}: {.val {constant_vars}}")
    }
    bma_data <- bma_data[, !is_constant, drop = FALSE]
    bma_var_names <- bma_var_names[!bma_var_names %in% constant_vars]

    if (ncol(bma_data) < 2) {
      cli::cli_abort("Not enough variables remaining after removing constants. Need at least effect and one moderator.")
    }
  }

  # Check for rank deficiency and multicollinearity, and remove aliased variables
  if (ncol(bma_data) > 2) {
    max_iterations <- 50  # Prevent infinite loop
    iteration <- 0

    repeat {
      iteration <- iteration + 1
      if (iteration > max_iterations) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning("Reached maximum iterations for collinearity removal")
        }
        break
      }

      # Build a design matrix to check rank
      formula_check <- stats::as.formula(paste("effect ~", paste(setdiff(names(bma_data), "effect"), collapse = " + ")))
      aliased_vars <- character(0)

      tryCatch(
        {
          lm_check <- stats::lm(formula_check, data = bma_data)
          aliased <- stats::alias(lm_check)

          if (!is.null(aliased$Complete) && length(aliased$Complete) > 0) {
            # Get coefficients with NA (these are the aliased ones)
            lm_coefs <- stats::coef(lm_check)
            aliased_vars <- names(lm_coefs)[is.na(lm_coefs)]
            aliased_vars <- aliased_vars[aliased_vars != "(Intercept)"]

            if (length(aliased_vars) > 0) {
              if (get_verbosity() >= 2 && iteration == 1) {
                cli::cli_alert_warning("Detected {length(aliased_vars)} aliased coefficient{?s} (perfect collinearity)")
                cli::cli_alert_info("Automatically removing collinear variables...")
              }

              # Get priority variables to preserve if possible
              box::use(artma / variable / bma[get_bma_priority_variables])
              priority_vars <- get_bma_priority_variables()

              # Remove one aliased variable, preferring non-priority ones
              non_priority_aliased <- setdiff(aliased_vars, priority_vars)
              var_to_remove <- if (length(non_priority_aliased)) {
                non_priority_aliased[1]
              } else {
                aliased_vars[1]
              }

              if (get_verbosity() >= 3) {
                cli::cli_alert_info("Removing collinear variable: {.val {var_to_remove}}")
              }

              # Remove the variable from bma_data and bma_var_names
              bma_data <- bma_data[, names(bma_data) != var_to_remove, drop = FALSE]
              bma_var_names <- bma_var_names[bma_var_names != var_to_remove]

              if (ncol(bma_data) < 2) {
                cli::cli_abort("Not enough variables remaining after removing collinear variables. Need at least effect and one moderator.")
              }

              # Continue loop to check if more aliased variables remain
              next
            }
          }
        },
        error = function(e) {
          if (get_verbosity() >= 3) {
            cli::cli_alert_info("Could not check for collinearity: {e$message}")
          }
        }
      )

      # No aliased variables found, exit loop
      if (!length(aliased_vars)) {
        break
      }
    }

    if (iteration > 1 && get_verbosity() >= 2) {
      cli::cli_alert_success("Removed {iteration - 1} collinear variable{?s}")
    }
  }

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

    extract_bma_results(
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
    var_names <- bma_model$reg.names

    # Extract coefficient matrix and get "Post Mean" column
    bma_coefs_all <- stats::coef(bma_model, order.by.pip = FALSE, exact = TRUE, include.constant = TRUE)

    if (is.matrix(bma_coefs_all) || length(dim(bma_coefs_all)) > 1) {
      post_means <- bma_coefs_all[, "Post Mean"]

      # Remove intercept row if present
      rnames <- rownames(bma_coefs_all)
      if (!is.null(rnames) && length(rnames) > 0) {
        intercept_idx <- which(rnames == "(Intercept)")
        if (length(intercept_idx) > 0) {
          post_means <- post_means[-intercept_idx]
        }
      }

      if (length(post_means) == length(var_names)) {
        coef_values <- as.numeric(post_means)
      } else {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning("Coefficient count mismatch: got {length(post_means)}, expected {length(var_names)}")
        }
        coef_values <- rep(NA, length(var_names))
      }
    } else {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Unexpected coefficient structure")
      }
      coef_values <- rep(NA, length(var_names))
    }

    coef_df <- data.frame(
      variable = var_names,
      pip = as.numeric(pip_values[var_names]),
      post_mean = coef_values,
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
    invisible(results[[1]])
  } else {
    invisible(results)
  }
}

#' Prompt user to select BMA variables at runtime
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data config
#' @return *\[character\]* Selected variable names
prompt_bma_variable_selection <- function(df, config) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / autonomy[should_prompt_user],
    artma / variable / bma[suggest_variables_for_bma]
  )

  if (!should_prompt_user(required_level = 4)) {
    if (get_verbosity() >= 3) {
      cli::cli_inform("Autonomy level is high - using automatic variable selection")
    }
    suggestions <- suggest_variables_for_bma(
      df,
      config = config,
      min_obs_per_split = 5,
      min_variance_ratio = 0.01,
      exclude_reference = TRUE
    )
    suggested_vars <- suggestions[suggestions$suggested, ]
    if (nrow(suggested_vars) > 0) {
      return(suggested_vars$var_name)
    }
    return(character(0))
  }

  # First, prompt for selection mode
  selection_mode <- prompt_bma_variable_selection_mode()

  selection <- if (selection_mode == "auto") auto_select_bma_variables(df, config) else manual_select_bma_variables(df, config)

  selection
}

#' Prompt for manual or auto variable selection mode
#' @return *\[character\]* Either "manual" or "auto"
prompt_bma_variable_selection_mode <- function() {
  box::use(
    artma / const[CONST],
    artma / libs / core / autonomy[should_prompt_user]
  )

  if (!should_prompt_user(required_level = 4)) {
    return("auto")
  }

  choices <- c(
    "Automatic detection (recommended)" = "auto",
    "Manual selection (interactive menu)" = "manual"
  )

  cli::cli_h1("BMA Variable Selection Mode")
  cli::cli_text("Choose how to select variables for Bayesian Model Averaging analysis.")
  cli::cat_line()

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Select variable selection mode",
    selected = 1 # "auto"
  )

  if (rlang::is_empty(selected)) {
    cli::cli_alert_info("No selection made. Using default: {CONST$STYLES$OPTIONS$VALUE('auto')}")
    return("auto")
  }

  selected_value <- choices[selected][[1]]
  cli::cli_alert_success("Selected mode: {CONST$STYLES$OPTIONS$VALUE(selected_value)}")
  cli::cat_line()

  selected_value
}

#' Automatically select BMA variables
#'
#' @description
#' Uses intelligent variable suggestion logic to automatically detect suitable
#' moderator variables for BMA analysis. Filters based on data type, variance,
#' observation counts, and collinearity. Prioritizes important variables like
#' standard error. Ensures no dummy variable traps or perfect collinearity.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data config
#' @return *\[character\]* Selected variable names
auto_select_bma_variables <- function(df, config) {
  box::use(
    artma / variable / bma[suggest_variables_for_bma],
    artma / libs / core / utils[get_verbosity]
  )

  cli::cli_h2("Automatic BMA Variable Suggestion")
  cli::cli_text("Analyzing variables to identify suitable moderators...")
  cli::cli_text("Checking for collinearity and prioritizing important variables...")
  cli::cat_line()

  # Use BMA-specific variable suggestion with collinearity checking
  suggestions <- suggest_variables_for_bma(
    df,
    config = config,
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  suggested_vars <- suggestions[suggestions$suggested, ]

  if (nrow(suggested_vars) == 0) {
    cli::cli_alert_warning("No variables were automatically suggested for BMA.")
    cli::cli_text("This may occur if:")
    cli::cli_ul(c(
      "Variables have insufficient observations",
      "Variables have very low variance",
      "No numeric variables are available"
    ))
    cli::cat_line()

    choices <- c("Yes, manual selection" = "yes", "No, skip BMA" = "no")
    use_manual_selection <- climenu::select(
      choices = names(choices),
      prompt = "Would you like to manually select variables instead?",
      selected = 1
    )
    use_manual <- !rlang::is_empty(use_manual_selection) &&
      choices[use_manual_selection] == "yes"

    if (use_manual) {
      return(manual_select_bma_variables(df, config))
    } else {
      return(character(0))
    }
  }

  # Display suggestions with reasoning
  cli::cli_alert_success("Found {nrow(suggested_vars)} suggested moderator{?s}:")
  cli::cat_line()

  for (i in seq_len(nrow(suggested_vars))) {
    var <- suggested_vars[i, ]
    var_name <- var$var_name
    var_reason <- var$reason

    # Get verbose name from config
    var_config <- config[[make.names(var_name)]]
    var_verbose <- if (!is.null(var_config$var_name_verbose)) {
      var_config$var_name_verbose
    } else {
      var_name
    }

    reason_text <- cli::col_silver(paste0("[", var_reason, "]"))
    cli::cli_text("{i}. {.field {var_verbose}} {reason_text}")
  }
  cli::cat_line()

  # Display important notes about BMA
  cli::cli_alert_info("Note: BMA will use these as continuous moderator variables")
  cli::cli_text("• Reference variables in dummy groups have been automatically excluded")
  cli::cli_text("• Consider enabling VIF optimization to handle multicollinearity")
  cli::cat_line()

  box::use(artma / libs / core / autonomy[should_prompt_user])
  if (!should_prompt_user(required_level = 4)) {
    var_names <- suggested_vars$var_name
    cli::cli_alert_success("Using {length(var_names)} suggested moderator{?s} (autonomy level: high)")
    return(var_names)
  }

  # Confirm suggestions
  confirm_choices <- c(
    "Yes, use these variables" = "yes",
    "No, select manually" = "no"
  )
  confirmation <- climenu::select(
    choices = names(confirm_choices),
    prompt = "Do you want to use these suggested variables for BMA?",
    selected = 1
  )
  confirmed <- !rlang::is_empty(confirmation) &&
    confirm_choices[confirmation] == "yes"

  if (!confirmed) {
    cli::cli_alert_info("Suggestions declined. Switching to manual selection...")
    cli::cat_line()
    return(manual_select_bma_variables(df, config))
  }

  cli::cli_alert_success("Using {nrow(suggested_vars)} suggested moderator{?s}")

  # Get variable names
  var_names <- suggested_vars$var_name

  # Prompt to save the selection
  box::use(artma / interactive / save_preference[prompt_save_variable_selection])

  # For BMA, we don't have detailed configs, just mark as BMA variables
  var_configs <- stats::setNames(
    lapply(var_names, function(v) list(var_name = v, use_in_bma = TRUE)),
    var_names
  )

  prompt_save_variable_selection(
    var_names = var_names,
    var_configs = var_configs,
    description = "BMA moderator variables",
    respect_autonomy = FALSE
  )

  # Return just the variable names (BMA doesn't need split specifications)
  var_names
}

#' Manual variable selection via interactive menu
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data config
#' @return *\[character\]* Selected variable names
manual_select_bma_variables <- function(df, config) {
  box::use(
    artma / const[CONST],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / autonomy[should_prompt_user]
  )

  if (!should_prompt_user(required_level = 4)) {
    if (get_verbosity() >= 3) {
      cli::cli_inform("Autonomy level is high - skipping manual variable selection")
    }
    return(character(0))
  }

  cli::cli_h1("BMA Variable Selection")
  cli::cat_line()

  if (is.null(config) || length(config) == 0) {
    cli::cli_alert_warning("No variables available for BMA analysis.")
    return(character(0))
  }

  # Get potential moderator variables (exclude only effect as dependent variable)
  excluded_vars <- c("effect")

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
    return_index = TRUE,
    allow_select_all = TRUE
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

#' Save BMA variables to data config
#' @param variables *\[character\]* Variable names to save
#' @param config *\[list\]* Current data config
save_bma_variables_to_data_config <- function(variables, config) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data_config / write[update_data_config]
  )

  if (length(variables) == 0) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      # Build changes: set bma=TRUE for selected variables, FALSE for others
      changes <- list()

      # First, set all existing variables to bma=FALSE (clear previous selections)
      for (var_name in names(config)) {
        if (is.list(config[[var_name]])) {
          changes[[var_name]] <- list(bma = FALSE)
        }
      }

      # Then set bma=TRUE for selected variables
      for (var_name in variables) {
        if (!is.list(changes[[var_name]])) {
          changes[[var_name]] <- list()
        }
        changes[[var_name]]$bma <- TRUE
      }

      # Update the data config
      update_data_config(changes)

      if (get_verbosity() >= 3) {
        cli::cli_alert_success("BMA variables saved to data config for future runs")
      }
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Could not save BMA variables to data config: {e$message}")
        cli::cli_alert_info("Variables will be used for this session only")
      }
    }
  )
}


box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  bma,
  stage = "bma",
  key_builder = function(...) build_data_cache_signature()
)

box::export(bma, run)
