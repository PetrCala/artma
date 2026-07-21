#' @title Bayesian Model Averaging utilities
#' @description
#' Shared functionality for Bayesian Model Averaging operations including
#' parameter handling, formula construction, VIF testing, data preparation,
#' model execution, and result extraction.

#' Split BMA parameters into sub-lists for multiple models
#'
#' @description
#' Split the BMA parameter list into sub-lists, where each of these has values of
#' length 1 and represents a single BMA model setup. The first model should always
#' appear in index one of parameters with multiple values. For parameters with
#' only one value, this value will be used for all models (sub-lists). In case
#' there are not either 1 or n values specified for each parameter (where n is
#' the number of models the user wishes to use), the code will throw an error.
#'
#' @note
#' The function always returns a list, even if all parameters have a single value.
#' This is so that the bma model main for loop can iterate over the results.
#'
#' @param bma_params *\[list\]* A list with the "bma_param_" parameters from the
#' user parameter file.
#'
#' @return A list of lists, where each sub-list corresponds to a single BMA
#' model setup.
#'
#' @export
handle_bma_params <- function(bma_params) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(is.list(bma_params))

  adj_bma_params <- list()
  param_counts <- unique(vapply(bma_params, length, integer(1)))

  if (length(param_counts) == 1) {
    adj_bma_params[[1]] <- bma_params
  } else if (length(param_counts) == 2) {
    model_count <- param_counts[!param_counts == 1]
    for (i in seq_len(model_count)) {
      single_model_params <- lapply(bma_params, function(x) x[1])
      adj_bma_params[[i]] <- single_model_params
      bma_params <- lapply(bma_params, function(x) {
        if (length(x) > 1) {
          return(x[-1])
        }
        x
      })
    }
  } else {
    cli::cli_abort("You must provide one or n values for each BMA parameter. n can be any number, but all parameters must have 1 or n values.")
  }

  adj_bma_params
}


#' Create a formula for Bayesian model averaging
#'
#' @description
#' Creates a formula for Bayesian model averaging based on the variables in input_var.
#' The formula includes the variables specified in input_var, with "effect" as the
#' dependent variable.
#'
#' @param input_var *\[character\]* A vector of variables that should be used to construct the formula.
#' Must include "effect". Other variables (including "se") are optional.
#' @param input_data *\[data.frame\]* A data frame on which the formula will later be used.
#' Skip adding any variables where all values of this data frame are 0 for the variable.
#' @param get_var_vector_instead *\[logical\]* If TRUE, return a vector with variable names instead,
#' with effect at the first position of the vector. Used for a simple rearrangement.
#' Defaults to FALSE.
#'
#' @return A formula object (to be used for) Bayesian model averaging
#'
#' @note To get the vector itself from the formula, you can use the in-built "all.vars()" method instead.
#'
#' @export
get_bma_formula <- function(input_var, input_data, get_var_vector_instead = FALSE) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(
    is.character(input_var),
    is.data.frame(input_data),
    is.logical(get_var_vector_instead)
  )

  # Separate effect (dependent variable) from independent variables
  independent_vars <- input_var[input_var != "effect"]

  # Remove variables with no variance (constant values)
  zero_vars <- names(input_data)[vapply(input_data, function(col) {
    length(unique(col)) == 1
  }, logical(1))]
  independent_vars <- independent_vars[!independent_vars %in% zero_vars]

  if (get_var_vector_instead) {
    return(c("effect", independent_vars))
  }

  # Build formula: effect ~ var1 + var2 + ...
  if (length(independent_vars) == 0) {
    # No independent variables, just intercept
    return(stats::as.formula("effect ~ 1"))
  }

  independent_vars_verbose <- paste(independent_vars, sep = "", collapse = " + ")
  all_vars_verbose <- paste0("effect ~ ", independent_vars_verbose)
  stats::as.formula(all_vars_verbose)
}


#' Search for an optimal Bayesian Model Averaging formula
#'
#' @description
#' This function searches for an optimal Bayesian Model Averaging (BMA) formula by removing the variables
#' with the highest Variance Inflation Factor (VIF) until the VIF coefficients of the remaining variables
#' are below 10 or the maximum number of groups to remove is reached.
#'
#' @param input_data *\[data.frame\]* A data frame containing the input data.
#' @param input_var_list *\[data.frame\]* A data frame containing the variable names, a boolean indicating
#' whether the variable is a potential variable for the model, and a grouping category for each variable.
#' @param max_groups_to_remove *\[integer\]* An integer indicating the maximum number of variable groups to remove.
#' @param return_variable_vector_instead *\[logical\]* A logical value indicating whether the function should return
#' a vector of remaining variables instead of the BMA formula.
#' @param verbose *\[logical\]* A logical value indicating whether the function should print the progress
#' and the suggested BMA formula.
#'
#' @return If return_variable_vector_instead is TRUE, the function returns a character vector of the remaining
#' variables. Otherwise, it returns a formula object of the suggested BMA formula. These are returned as a list
#' along with three other performance indicators (used in verbose output and caching).
#'
#' @export
find_optimal_bma_formula <- function(input_data, input_var_list, max_groups_to_remove = 30,
                                     return_variable_vector_instead = FALSE, verbose = TRUE) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity]
  )

  validate(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.numeric(max_groups_to_remove),
    is.logical(return_variable_vector_instead),
    is.logical(verbose),
    all(c("bma", "var_name", "group_category") %in% colnames(input_var_list))
  )

  if (!requireNamespace("car", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg car} is required for BMA collinearity checks. Install with: install.packages('car')")
  }

  input_data <- input_data[, colnames(input_data) %in% input_var_list$var_name, drop = FALSE]

  non_const_cols <- vapply(input_data, function(col) {
    length(unique(col)) > 1
  }, logical(1))
  input_data <- input_data[, non_const_cols, drop = FALSE]

  bma_potential_vars_bool <- input_var_list$bma & non_const_cols
  potential_vars <- input_var_list$var_name[bma_potential_vars_bool]
  var_grouping <- input_var_list$group_category[bma_potential_vars_bool]

  var_grouping <- var_grouping[!potential_vars == "effect"]
  potential_vars <- potential_vars[!potential_vars == "effect"]

  bma_formula <- get_bma_formula(potential_vars, input_data)
  bma_lm <- stats::lm(bma_formula, data = input_data)
  vif_coefs <- car::vif(bma_lm)

  if (length(var_grouping) != length(vif_coefs)) {
    cli::cli_abort("The lengths of the variable vectors do not match")
  }

  removed_groups <- 0
  removed_groups_verbose <- character(0)

  while (any(vif_coefs > 10) && max_groups_to_remove > 0) {
    highest_vif_coef_name <- names(which.max(vif_coefs))
    highest_vif_coef_idx <- which(potential_vars == highest_vif_coef_name)
    highest_vif_group <- var_grouping[highest_vif_coef_idx]

    vars_to_remove <- potential_vars[var_grouping == highest_vif_group]
    potential_vars <- potential_vars[!potential_vars %in% vars_to_remove]
    var_grouping <- var_grouping[!var_grouping %in% highest_vif_group]

    bma_formula <- get_bma_formula(potential_vars, input_data)
    bma_lm <- stats::lm(bma_formula, data = input_data)
    vif_coefs <- car::vif(bma_lm)

    if (length(var_grouping) != length(vif_coefs)) {
      cli::cli_abort("The lengths of the variable vectors do not match")
    }

    max_groups_to_remove <- max_groups_to_remove - 1
    removed_groups <- removed_groups + 1
    removed_groups_verbose <- c(removed_groups_verbose, vars_to_remove)
  }

  if (max_groups_to_remove == 0) {
    cli::cli_abort("Maximum number of groups to remove reached. Optimal BMA formula not found.")
  }

  res_object <- if (return_variable_vector_instead) {
    potential_vars
  } else {
    bma_formula
  }

  out_list <- list(
    result = res_object,
    vif_coefs = vif_coefs,
    removed_groups = removed_groups,
    removed_groups_verbose = removed_groups_verbose,
    formula = bma_formula
  )

  if (verbose && get_verbosity() >= 3) {
    cli::cli_alert_info("VIF Coefficients:")
    cli::cat_print(out_list$vif_coefs)
    cli::cli_alert_info("Removed {removed_groups} group{?s} with VIF > 10")
    if (length(removed_groups_verbose) > 0) {
      cli::cli_alert_info("Removed variables: {.val {removed_groups_verbose}}")
    }
    cli::cli_alert_success("Suggested BMA formula:")
    cli::cat_print(out_list$formula)
  }

  out_list
}


#' Get the data for Bayesian Model Averaging
#'
#' @description
#' An explicit function to subset the main data frame onto only those columns that are used
#' during the BMA estimation. The function is explicit for the simple reason that one of the
#' plots in the extract_bma_results requires the data object, so this function allows for that
#' object to exist outside the scope of the run_bma function, where it would be otherwise hidden.
#'
#' @param input_data *\[data.frame\]* A data from containing the BMA data (and more)
#' @param input_var_list *\[data.frame\]* A data frame containing the variable information.
#' @param variable_info *\[data.frame | character\]* Either a data frame containing the variable information,
#' or a vector of variables. In the latter case, the "from_vector" variable must be set to TRUE.
#' @param scale_data *\[logical\]* If TRUE, scale the data onto the same scale. Defaults to TRUE.
#' @param from_vector *\[logical\]* If True, the "variable_info" must be specified as a vector, otherwise
#' as a data frame. Defaults to FALSE.
#' @param include_reference_groups *\[logical\]* If TRUE, add the reference groups to the data. Be very
#' careful, as this may create a dummy trap. Used when creating the descriptive table of all potential
#' BMA variables. Usable only when from_vector == FALSE. Defaults to FALSE.
#'
#' @note When transforming/subsetting the data, there is a need to convert the data into a
#' data.frame object, otherwise the plot functions will not recognize the data types correctly
#' later on. The "bms" function works well even with a tibble, but the plots do not.
#'
#' @export
get_bma_data <- function(input_data, input_var_list, variable_info, scale_data = TRUE,
                         from_vector = TRUE, include_reference_groups = FALSE) {
  box::use(
    artma / libs / core / validation[validate, assert]
  )

  validate(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    any(
      is.data.frame(variable_info),
      is.vector(variable_info)
    ),
    is.logical(from_vector)
  )

  assert(!from_vector || is.vector(variable_info), "You must provide a vector if you wish to extract the variable information from a vector.")
  assert(from_vector || is.data.frame(variable_info), "You must provide a data frame if you wish to extract the variable information from a data frame.")

  if (is.data.frame(variable_info)) {
    desired_vars_bool <- variable_info$bma
    if (include_reference_groups) {
      ref_bool <- variable_info$bma_reference_var
      desired_vars_bool <- desired_vars_bool | ref_bool
    }
    desired_vars <- variable_info$var_name[desired_vars_bool]
  } else {
    desired_vars <- variable_info
  }

  bma_data <- as.data.frame(input_data[desired_vars])

  for (column in colnames(bma_data)) {
    row_idx <- match(column, input_var_list$var_name)
    to_log <- as.logical(input_var_list[row_idx, "to_log_for_bma"])
    if (isTRUE(to_log)) {
      bma_data[, column] <- log(bma_data[, column])
      bma_data[is.infinite(bma_data[, column]), column] <- 0
    }
  }

  if (scale_data) {
    source_colnames <- colnames(bma_data)
    is_binary <- function(x) {
      length(unique(x)) == 2
    }
    binary_cols <- vapply(bma_data, is_binary, logical(1))
    bma_data[, !binary_cols] <- lapply(bma_data[, !binary_cols, drop = FALSE], function(x) {
      as.numeric(scale(x))
    })
    colnames(bma_data) <- source_colnames
  }

  bma_data
}


#' Run a Bayesian model averaging estimation
#'
#' @description
#' Input the BMA data, the variable information data frame
#' and inherited parameters, which are all the parameters you want to use for the actual
#' estimation inside the 'bms' function. Validate correct input, run the estimation, and
#' return the BMA model without printing any results.
#'
#' @param bma_data *\[data.frame\]* The data for BMA. "effect" must be in the first column.
#' @param bma_params *\[list\]* Parameters to be used inside the "bms" function. These are:
#' burn, iter, g, mprior, nmodel, mcmc. For more info see the "bms" function documentation.
#'
#' @return The bma model object
#'
#' @export
run_bma <- function(bma_data, bma_params, quiet = TRUE) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(
    is.data.frame(bma_data),
    !any(is.na(bma_data)),
    all(vapply(bma_data, is.numeric, logical(1))),
    colnames(bma_data)[1] == "effect"
  )

  if (!requireNamespace("BMS", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg BMS} is required for Bayesian model averaging. Install with: install.packages('BMS')")
  }

  all_bma_params <- c(
    list(bma_data),
    bma_params
  )

  tryCatch(
    {
      grDevices::dev.off()
    },
    error = function(e) {
      # Ignore errors when closing graphics device
    }
  )

  set.seed(123)
  if (quiet) {
    invisible(utils::capture.output(
      bma_model <- do.call(BMS::bms, all_bma_params)
    ))
  } else {
    bma_model <- do.call(BMS::bms, all_bma_params)
  }

  bma_model
}


#' Rename the BMA model names to their verbose form
#'
#' @description
#' Rename the BMA model names to their verbose form using the variable information
#' data frame. Input these two objects (BMA model and variable list DF) and return
#' the modified BMA model.
#'
#' @param bma_model *\[bma\]* BMA model object
#' @param input_var_list *\[data.frame\]* Variable information data frame
#'
#' @return Modified BMA model with verbose names
#'
#' @export
rename_bma_model <- function(bma_model, input_var_list) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(
    inherits(bma_model, "bma"),
    is.data.frame(input_var_list)
  )

  bma_names <- bma_model$reg.names
  idx <- match(bma_names, input_var_list$var_name)
  bma_names[!is.na(idx)] <- input_var_list$var_name_verbose[stats::na.omit(idx)]
  bma_names[is.na(idx)] <- "Intercept"
  bma_model$reg.names <- bma_names

  bma_model
}


#' Extract results from a Bayesian Model Averaging regression
#'
#' @description
#' Extract coefficients and optionally create diagnostic plots for a BMA model.
#' The function supports multiple print levels and can export graphics.
#'
#' @param bma_model *\[bma\]* An object of class bma containing the BMA regression model.
#' @param bma_data *\[data.frame\]* A data frame containing the data used to fit the BMA model.
#' @param input_var_list *\[data.frame\]* A data frame with the variable information.
#' @param print_results *\[character\]* A character value indicating the level of result printing desired.
#' Can be one of: "none", "fast", "verbose", "all", "table"
#' @param theme *\[character\]* Color theme name. Defaults to "blue".
#' @param export_graphics *\[logical\]* If TRUE, export the graphs into the graphics folder. Defaults to TRUE.
#' @param export_path *\[character\]* Path to the export folder. Defaults to "graphics".
#' @param graph_scale *\[numeric\]* Scale the corrplot graph by this number. Defaults to 1.
#'
#' @return A numeric vector containing only the BMA coefficients.
#'
#' @export
extract_bma_results <- function(bma_model, bma_data, input_var_list, print_results = "fast",
                                theme = "blue", export_graphics = TRUE,
                                export_path = "graphics", graph_scale = 1) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / visualization / colors[get_colors]
  )

  validate(
    inherits(bma_model, "bma"),
    is.data.frame(bma_data),
    is.data.frame(input_var_list),
    is.character(print_results),
    is.character(theme),
    print_results %in% c("none", "fast", "verbose", "all", "table"),
    is.logical(export_graphics),
    is.character(export_path),
    is.numeric(graph_scale)
  )

  bma_model <- rename_bma_model(bma_model, input_var_list)

  effect_verbose <- input_var_list$var_name_verbose[match("effect", input_var_list$var_name)]
  bma_matrix_names <- c(effect_verbose, bma_model$reg.names)

  bma_coefs <- stats::coef(bma_model, order.by.pip = FALSE, exact = TRUE, include.constant = TRUE)

  if (get_verbosity() >= 3 && print_results != "none") {
    cli::cli_h3("Bayesian Model Averaging Results")
  }

  if (print_results %in% c("verbose", "all")) {
    cli::cat_print(bma_model)
    cli::cat_print(bma_model$topmod[1])
  } else if (print_results == "fast") {
    if (get_verbosity() >= 3) {
      cli::cat_print(bma_coefs)
    }
  }

  if (any(print_results == "all", export_graphics)) {
    color_spectrum <- get_colors(theme, "bma")

    main_plot_call <- bquote(
      graphics::image(bma_model,
        col = .(color_spectrum), yprop2pip = FALSE, order.by.pip = TRUE, # nolint: box_usage_linter. # bquote substitution
        do.par = TRUE, do.grid = TRUE, do.axis = TRUE, xlab = "", main = ""
      )
    )

    dist_color_spectrum <- color_spectrum[color_spectrum != "white"]
    bma_dist_call <- bquote(
      base::plot(bma_model, col = .(dist_color_spectrum)) # nolint: box_usage_linter. # bquote substitution
    )

    has_corrplot <- requireNamespace("corrplot", quietly = TRUE)
    if (!has_corrplot) {
      cli::cli_warn("Package {.pkg corrplot} is not installed; skipping the BMA correlation plot. Install with: install.packages('corrplot')")
    }

    bma_matrix <- stats::cor(bma_data)
    dimnames(bma_matrix) <- lapply(dimnames(bma_matrix), function(x) {
      bma_matrix_names
    })
    bma_col <- grDevices::colorRampPalette(color_spectrum)
    corrplot_mixed_call <- quote(
      corrplot::corrplot.mixed(bma_matrix,
        lower = "number", upper = "circle",
        lower.col = bma_col(200), upper.col = bma_col(200), tl.pos = c("lt"),
        diag = c("u"), tl.col = "black", tl.srt = 70, tl.cex = 0.55,
        number.cex = 0.5, cl.cex = 0.8, cl.ratio = 0.1
      )
    )

    # BMS:::image.bma drops a matrix dimension for small models and errors for
    # 2 to 3 regressors, so only attempt the image plot once there are >= 4.
    n_regressors <- length(bma_model$reg.names)
    image_supported <- n_regressors >= 4L

    plot_env <- environment()
    safe_render_plot <- function(plot_call, description) {
      tryCatch(
        eval(plot_call, envir = plot_env),
        error = function(e) {
          cli::cli_warn("Could not render the BMA {description}: {conditionMessage(e)}")
        }
      )
    }
    render_bma_image <- function() {
      if (!image_supported) {
        cli::cli_alert_info(
          "Skipping the BMA image plot: it needs at least 4 regressors (model has {n_regressors})."
        )
        return(invisible(NULL))
      }
      safe_render_plot(main_plot_call, "image plot")
    }
  }

  if (print_results == "all" && get_verbosity() >= 3) {
    cli::cli_alert_info("Printing BMA plots...")
    render_bma_image()
    safe_render_plot(bma_dist_call, "distribution plot")
    if (has_corrplot) {
      safe_render_plot(corrplot_mixed_call, "correlation plot")
    }
  }

  if (export_graphics) {
    box::use(artma / visualization / export[ensure_export_dir])

    gprior <- bma_model$gprior.info$gtype
    mprior <- bma_model$mprior.info$origargs$mpmode

    ensure_export_dir(export_path)

    main_path <- file.path(export_path, paste0("bma_", gprior, "_", mprior, "_results.png"))
    dist_path <- file.path(export_path, paste0("bma_", gprior, "_", mprior, "_dist.png"))
    corrplot_path <- file.path(export_path, paste0("bma_", gprior, "_", mprior, "_corrplot.png"))

    for (path in list(main_path, dist_path, corrplot_path)) {
      if (file.exists(path)) {
        file.remove(path)
      }
    }

    if (image_supported) {
      grDevices::png(main_path,
        width = 933 * graph_scale, height = 894 * graph_scale, units = "px",
        res = 70 * graph_scale
      )
      safe_render_plot(main_plot_call, "image plot")
      grDevices::dev.off()
    } else {
      cli::cli_alert_info(
        "Skipping the BMA image plot export: it needs at least 4 regressors (model has {n_regressors})."
      )
    }

    grDevices::png(dist_path,
      width = 528 * graph_scale, height = 506 * graph_scale, units = "px",
      res = 90 * graph_scale
    )
    safe_render_plot(bma_dist_call, "distribution plot")
    grDevices::dev.off()

    if (has_corrplot) {
      grDevices::png(corrplot_path,
        width = 700 * graph_scale, height = 669 * graph_scale, units = "px",
        res = 90 * graph_scale
      )
      safe_render_plot(corrplot_mixed_call, "correlation plot")
      grDevices::dev.off()
    }
  }

  bma_coefs
}


#' Build human-readable labels distinguishing multiple BMA parameter sets
#'
#' @description
#' Given the per-model parameter lists returned by `handle_bma_params()`, build a
#' label for each model listing only the parameters that actually differ between
#' them (e.g. "g=UIP, mprior=uniform"). Falls back to "Model N" if every parameter
#' set is identical.
#'
#' @param bma_params_list *\[list\]* A list of per-model parameter lists, as returned
#' by `handle_bma_params()`.
#'
#' @return *\[character\]* A vector of labels, one per entry in `bma_params_list`.
#'
#' @export
build_bma_model_labels <- function(bma_params_list) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(is.list(bma_params_list), length(bma_params_list) >= 1)

  if (length(bma_params_list) == 1) {
    return("Model 1")
  }

  param_names <- names(bma_params_list[[1]])
  varying_params <- Filter(function(param_name) {
    values <- vapply(bma_params_list, function(params) as.character(params[[param_name]]), character(1))
    length(unique(values)) > 1
  }, param_names)

  if (!length(varying_params)) {
    return(paste0("Model ", seq_along(bma_params_list)))
  }

  vapply(bma_params_list, function(params) {
    paste(
      vapply(varying_params, function(param_name) {
        paste0(param_name, "=", params[[param_name]])
      }, character(1)),
      collapse = ", "
    )
  }, character(1))
}


#' Render a BMA prior-comparison plot across multiple parameter sets
#'
#' @description
#' When more than one BMA parameter set is run (e.g. to compare priors), overlay
#' their posterior statistics with `BMS::plotComp` so the effect of different
#' configurations can be compared visually.
#'
#' @param bma_models *\[list\]* A named list of two or more fitted `bma` model objects
#' to compare. The names are used as the plot legend labels.
#' @param comp *\[character\]* The statistic to compare: "PIP", "Post Mean", "Post SD",
#' "Std Mean", or "Std SD". Defaults to "PIP".
#' @param export_graphics *\[logical\]* If TRUE, export the comparison plot to a file. Defaults to TRUE.
#' @param export_path *\[character\]* Path to the export folder. Defaults to "graphics".
#' @param graph_scale *\[numeric\]* Scale the plot by this number. Defaults to 1.
#' @param print_plot *\[logical\]* If TRUE, also render the plot to the current graphics device. Defaults to FALSE.
#'
#' @return NULL (invisibly). Called for its plotting side effect.
#'
#' @export
render_bma_comparison_plot <- function(bma_models, comp = "PIP", export_graphics = TRUE,
                                       export_path = "graphics", graph_scale = 1,
                                       print_plot = FALSE) {
  box::use(
    artma / libs / core / validation[validate],
    artma / visualization / export[ensure_export_dir]
  )

  validate(
    is.list(bma_models),
    length(bma_models) >= 2,
    all(vapply(bma_models, inherits, logical(1), what = "bma")),
    is.character(comp),
    is.logical(export_graphics),
    is.character(export_path),
    is.numeric(graph_scale),
    is.logical(print_plot)
  )

  safe_render <- function() {
    tryCatch(
      do.call(BMS::plotComp, c(bma_models, list(comp = comp))),
      error = function(e) {
        cli::cli_warn("Could not render the BMA comparison plot: {conditionMessage(e)}")
      }
    )
  }

  if (print_plot) {
    safe_render()
  }

  if (export_graphics) {
    ensure_export_dir(export_path)
    comparison_path <- file.path(export_path, "bma_comparison.png")
    if (file.exists(comparison_path)) {
      file.remove(comparison_path)
    }
    grDevices::png(comparison_path,
      width = 800 * graph_scale, height = 600 * graph_scale, units = "px",
      res = 90 * graph_scale
    )
    safe_render()
    grDevices::dev.off()
  }

  invisible(NULL)
}


box::export(
  handle_bma_params,
  get_bma_formula,
  find_optimal_bma_formula,
  get_bma_data,
  run_bma,
  rename_bma_model,
  extract_bma_results,
  build_bma_model_labels,
  render_bma_comparison_plot
)
