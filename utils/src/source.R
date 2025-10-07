######################### MODEL AVERAGING #########################

###### HETEROGENEITY - Bayesian Model Averaging in R ######

#' @title Handle BMA setup parameters for multiple models
#'
#' @description
#' Split the BMA parameter list into sub-lists, where each of these has values of
#'  length 1 and represents a single BMA model setup. The first model should always
#'  appear in index one of parameters with multiple values. For parameters with
#'  only one value, this value will be used for all models (sub-lists). In case
#'  there are not either 1 or n values specified for each parameter (where n is
#'  the number of models the user wishes to use), the code will throw an error.
#'
#' @note
#' The function always returns a list, even if all parameters have a single value.
#'  This is so that the bma model main for loop can iterate over the results.
#'
#' @param bma_params [list] A list with the "bma_param_" parameters from the
#'  user parameter file.
#'
#' @return A list of lists, where each sub-list corresponds to a single BMA
#'  model setup.
handleBMAParams <- function(bma_params) {
  adj_bma_params <- list() # Store results here
  param_counts <- unique(sapply(bma_params, length)) # Values per parameter
  if (length(param_counts) == 1) {
    adj_bma_params[[1]] <- bma_params
  } else if (length(param_counts) == 2) {
    model_count <- param_counts[!param_counts == 1]
    # Iterate over models parameter values - evaluate main model first with index 1
    for (i in 1:model_count) {
      # Extract parameters of the current iteration model
      single_model_params <- lapply(bma_params, function(x) {
        x[1]
      })
      adj_bma_params[[i]] <- single_model_params
      # Remove the last value for parameters with multiple values
      bma_params <- lapply(bma_params, function(x) {
        if (length(x) > 1) {
          return(x[-1]) # All but first element
        }
        return(x)
      })
    }
  } else {
    stop("You must provide one or n values for each BMA parameter. n can be any number, but all parameters must have 1 or n values.")
  }
  return(adj_bma_params)
}

#' This function searches for an optimal Bayesian Model Averaging (BMA) formula by removing the variables
#' with the highest Variance Inflation Factor (VIF) until the VIF coefficients of the remaining variables
#' are below 10 or the maximum number of groups to remove is reached.
#'
#' @param input_data A data frame containing the input data.
#' @param input_var_list A data frame containing the variable names, a boolean indicating whether the variable
#' is a potential variable for the model, and a grouping category for each variable.
#' @param max_groups_to_remove An integer indicating the maximum number of variable groups to remove.
#' @param return_variable_vector_instead A logical value indicating whether the function should return
#' a vector of remaining variables instead of the BMA formula.
#' @param verbose A logical value indicating whether the function should print the progress and the suggested BMA formula.
#'
#' @return If return_variable_vector_instead is TRUE, the function returns a character vector of the remaining variables.
#' Otherwise, it returns a formula object of the suggested BMA formula. These are returned as a list along with
#' three other performance indicators (used in verbose output and cacheing).
findOptimalBMAFormula <- function(input_data, input_var_list, max_groups_to_remove = 30,
                                  return_variable_vector_instead = F, verbose = T) {
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.numeric(max_groups_to_remove),
    is.logical(return_variable_vector_instead),
    is.logical(verbose),
    all(c("bma", "var_name", "group_category") %in% colnames(input_var_list))
  )
  # Subset input data to only columns defined in variable list
  input_data <- input_data[, colnames(input_data) %in% input_var_list$var_name]
  # Remove any variables for which all values in data are the same
  non_const_cols <- apply(input_data, 2, function(col) {
    length(unique(col)) > 1
  })
  input_data <- input_data[, non_const_cols]
  # Extract the information from source data
  bma_potential_vars_bool <- input_var_list$bma & non_const_cols # BMA OK and non constant
  potential_vars <- input_var_list$var_name[bma_potential_vars_bool]
  var_grouping <- input_var_list$group_category[bma_potential_vars_bool]

  # Pop the effect from var grouping and potential vars (not used in the iteration)
  var_grouping <- var_grouping[!potential_vars == "effect"]
  potential_vars <- potential_vars[!potential_vars == "effect"]

  # Get initial BMA formula and VIF coefficients
  bma_formula <- getBMAFormula(potential_vars, input_data)
  bma_lm <- lm(bma_formula, data = input_data)
  vif_coefs <- car::vif(bma_lm)
  if (length(var_grouping) != length(vif_coefs)) { # 1 less variable in VIF coefs
    stop("The lengths of the variable vectors do not match")
  }

  removed_groups <- 0
  removed_groups_verbose <- c()
  while (any(vif_coefs > 10) && max_groups_to_remove > 0) {
    # Get the group with the highest VIF coefficient
    highest_vif_coef_name <- names(which.max(vif_coefs)) # Name of the coefficient with highest VIF
    highest_vif_coef_idx <- which(potential_vars == highest_vif_coef_name) # Index of the highest VIF coef
    highest_vif_group <- var_grouping[highest_vif_coef_idx] # Index of group to remove
    # Get new potential vars, new grouping
    vars_to_remove <- potential_vars[var_grouping == highest_vif_group]
    potential_vars <- potential_vars[!potential_vars %in% vars_to_remove]
    var_grouping <- var_grouping[!var_grouping %in% highest_vif_group]
    # Get modified BMA formula and VIF coefficients
    bma_formula <- getBMAFormula(potential_vars, input_data)
    bma_lm <- lm(bma_formula, data = input_data)
    vif_coefs <- car::vif(bma_lm)
    if (length(var_grouping) != length(vif_coefs)) {
      stop("The lengths of the variable vectors do not match")
    }
    # Decrease the maximum number of groups to remove
    max_groups_to_remove <- max_groups_to_remove - 1
    removed_groups <- removed_groups + 1
    removed_groups_verbose <- append(removed_groups_verbose, vars_to_remove)
  }
  # Print out the information about the procedure outcome
  if (max_groups_to_remove == 0) {
    stop("Maximum number of groups to remove reached. Optimal BMA formula not found.")
  }
  # Get main object to return - explicit because ifelse() does not work for some RRRRRReason
  if (return_variable_vector_instead) {
    res_object <- potencial_vars
  } else {
    res_object <- bma_formula
  }
  # All information to return (for cacheing)
  out_list <- list(res_object, vif_coefs, removed_groups, removed_groups_verbose, bma_formula)
  # Verbose output
  if (verbose) {
    findOptimalBMAFormulaVerbose(
      out_list, # Object plus four verbose indicators
      verbose = verbose
    )
  }
  # Return the outcome
  return(out_list)
}

#' Verbose output for the findOptimalBMAFormula function
findOptimalBMAFormulaVerbose <- function(out_list, ...) {
  args <- list(...)
  verbose_on <- args$verbose
  # Validate input
  stopifnot(
    is(out_list, "list"),
    length(out_list) == 5 # Via the main function
  )
  # Extract function output
  vif_coefs <- out_list[[2]]
  removed_groups <- out_list[[3]]
  removed_groups_verbose <- out_list[[4]]
  bma_formula <- out_list[[5]]
  if (verbose_on) {
    print("These are all the Variance Inflation Coefficients for this formula:")
    print(vif_coefs)
    print(paste("Removed", removed_groups, "groups with VIF > 10."))
    print("The removed groups contained these variables:")
    print(removed_groups_verbose)
    print("The suggested BMA formula is:")
    print(bma_formula)
  }
}

#' Creates a formula for Bayesian model averaging
#'
#' This function creates a formula for Bayesian model averaging based on the variables in \code{var_vector}.
#' The formula includes the variables "effect" and "se", as well as any other variables specified in \code{var_vector}.
#'
#' @param input_var [vector] A vector of variables that should be used to construct the formula. Must include
#' "effect" and "se".
#' @param input_data [data.frame] A data frame on which the formula will later be used. Skip adding any variables
#'  where all values of this data frame are 0 for the variable.
#' @param get_var_vector_instead [bool] If TRUE, return a vector with variable names instead, with effect and se
#'  at the first two positions of the vector. Used for a simple rearrangement. Defaults to FALSE.
#' @return A formula object (to be used) Bayesian model averaging
#'
#' @note To get the vector itself from the formula, you can use the in-built "all.vars()" method instead.
getBMAFormula <- function(input_var, input_data, get_var_vector_instead = F) {
  # Separate the effect and SE from the remaining variables
  bool_wo_effect <- input_var != "effect" # Pop effect
  bool_wo_se <- input_var != "se" # Pop se
  remaining_vars <- input_var[bool_wo_effect & bool_wo_se] # Remaining variables
  # Remove any variables for which all values in data are the same
  zero_vars <- input_data %>%
    select_if(~ length(unique(.)) == 1) %>%
    names()
  remaining_vars <- remaining_vars[!remaining_vars %in% zero_vars]
  # Get the formula
  if (get_var_vector_instead) {
    var_vector <- c("effect", "se", remaining_vars)
    return(var_vector)
  }
  remaining_vars_verbose <- paste(remaining_vars, sep = "", collapse = " + ")
  all_vars_verbose <- paste0("effect ~ se + ", remaining_vars_verbose)
  bma_formula <- as.formula(all_vars_verbose)
  return(bma_formula)
}

#' Function to test the Variance Inflation Factor of a Linear Regression Model
#'
#' @details runVifTest is a function that tests the Variance Inflation Factor (VIF) of a linear regression model.
#' It takes three arguments: input_, and print_all_coefs. The function tests whether the input_ is either a vector
#' or a formula. If it is a vector of variables, it transforms it into a formula. Then, it calculates the VIF coefficients
#' using the vif function from the car package. If print_all_coefs is set to TRUE, the function prints all the VIF
#' coefficients. If any of the VIF coefficients is larger than 10, the function prints a message indicating the
#' variables with a high VIF. Otherwise, it prints a message indicating that all variables have a VIF lower than 10.
#' Finally, the function returns the VIF coefficients as a numeric vector.
#'
#' @note If you input the formula, all data for these variables must be a vector with at least some variation.
#' Otherwise the function will return an error.
#'
#' @param input_var [vector | formula] One of - vector of variable names, formula. If it is a vector, the function
#' transforms the input into a formula.
#' @param input_data [data.frame] Data to run the test on.
#' @param print_all_coefs [bool] A logical value indicating whether to print all the VIF coefficients into
#'  the console
#' @param verbose [bool] If TRUE, print out the information about the output. Defaults to TRUE.
#'
#' @return [vector] A numeric vector with the VIF coefficients.
runVifTest <- function(input_var, input_data, print_all_coefs = F, verbose = T) {
  # Validate input
  stopifnot(
    any(
      is_formula(input_var),
      is.vector(input_var)
    ),
    is.data.frame(input_data)
  )
  # Get the BMA formula - explicit because RRRRRR
  if (is.vector(input_var)) {
    BMA_formula <- getBMAFormula(input_var) # Automatically validates that all vectors are non-0
  } else {
    if (nrow(input_data %>% select_if(~ length(unique(.)) > 1)) < nrow(input_data)) {
      stop("All data must have at least some variation.")
    }
    BMA_formula <- input_var # Formula is valid
  }
  # Run the test
  BMA_reg_test <- lm(formula = BMA_formula, data = input_data)
  # Check that there are no NAs in the model
  if (any(is.na(coef(BMA_reg_test)))) {
    problematic_vars <- names(coef(BMA_reg_test))[which(is.na(coef(BMA_reg_test)))]
    message(paste(
      "There are some aliased coefficients in one of the suggested BMA model configurations.",
      "Check colinearity in the data, remove the correlated variables, or try changing the model.",
      "These are the problematic variables for the model:",
      paste(problematic_vars, collapse = ", "),
      "Note that the problem may lie elsewhere too, so removing these variables may not necessarily help.",
      sep = "\n"
    ))
    stop("Aliased coefficients in a suggested BMA model.")
  }
  # Unhandled exception - fails in case of too few observations vs. too many variables
  vif_coefs <- car::vif(BMA_reg_test) # VIF coefficients
  if (verbose) {
    if (print_all_coefs) {
      print("These are all the Variance Inflation Coefficients for this formula:")
      print(vif_coefs)
    }
    if (any(vif_coefs > 10)) {
      coefs_above_10_vif <- names(vif_coefs)[vif_coefs > 10]
      print("These variables have a Variance Inflation Coefficient larger than 10:")
      print(coefs_above_10_vif)
    } else {
      print("All BMA variables have a Variance Inflation Factor lower than 10. All good to go.")
    }
  }
  return(vif_coefs)
}


#' Get the data for Bayesian Model Averaging
#'
#' @details An explicit function to subset the main data frame onto only those columns that are used
#' during the BMA estimation. The function is explicit for the simple reason that one of the
#' plots in the extractBMAResults requires the data object, so this function allows for that
#' object to exist outside the scope of the runBMA function, where it would be otherwise hidden.
#'
#' @param input_data [data.frame] A data from containing the BMA data (and more)
#' @param input_var_list [data.frame] A data frame containing the variable information.
#' @param variable_info [data.frame | vector] Either a data frame containing the variable information,
#'  or a vector of variables. In the latter case, the "from_vector" variable must be set to T.
#' @param scale_data [logical] If TRUE, scale the data onto the same scale. Defaults to T.
#' @param from_vector [logical] If True, the "variable_info" must be specified as a vector, otherwise
#'  as a data frame. Defaults to FALSE.
#' @param include_reference_groups [logical] If TRUE, add the reference groups to the data. Be very
#' careful, as this may create a dummy trap. Used when creating the descriptive table of all potential
#' BMA variables. Usable only when from_vector == FALSE. Defaults to FALSE.
#' @note When transforming/subsetting the data, there is a need to convert the data into a
#' data.frame object, otherwise the plot functions will not recognize the data types correctly
#' later on. The "bms" function works well even with a tibble, but the plots do not. RRRRRRR
getBMAData <- function(input_data, input_var_list, variable_info, scale_data = T, from_vector = T,
                       include_reference_groups = F) {
  # Input validation
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    any(
      is.data.frame(variable_info),
      is.vector(variable_info)
    ),
    is.logical(from_vector)
  )
  # Subset the data
  if (from_vector && !is.vector(variable_info)) {
    stop("You must provide a vector if you wish to extract the variable information form a vector.")
  }
  if (!from_vector && !is.data.frame(variable_info)) {
    stop("You must provide a data frame if you wish to extract the variable information form a data frame.")
  }
  if (is.data.frame(variable_info)) { # Input data frame
    desired_vars_bool <- variable_info$bma
    if (include_reference_groups) {
      ref_bool <- variable_info$bma_reference_var
      desired_vars_bool <- desired_vars_bool | ref_bool # Add reference variables
    }
    desired_vars <- variable_info$var_name[desired_vars_bool]
  } else { # Input vector
    desired_vars <- variable_info
  }
  bma_data <- input_data[desired_vars] # Only desired variables
  bma_data <- as.data.frame(bma_data) # To a data.frame object, because RRRR

  # Convert all specified columns to logs - sub-optimal approach
  for (column in colnames(bma_data)) {
    row_idx <- match(column, input_var_list$var_name)
    to_log <- as.logical(input_var_list[row_idx, "to_log_for_bma"])
    if (to_log) {
      bma_data[, column] <- log(bma_data[, column])
      bma_data[is.infinite(bma_data[, column]), column] <- 0 # Replace infinite values with 0
    }
  }

  # Standardize non-binary data onto similar and directly comparable scale
  if (scale_data) {
    # Store source column names
    source_colnames <- colnames(bma_data)
    # Check whether the variables are binary or not
    is_binary <- function(x) {
      length(unique(x)) == 2
    }
    binary_cols <- sapply(bma_data, is_binary) # Boolean where TRUE if col is binary
    bma_data[, !binary_cols] <- lapply(bma_data[, !binary_cols], function(x) {
      as.numeric(scale(x))
    }) # Scale non-binary cols
    # Restore column names
    colnames(bma_data) <- source_colnames
  }

  return(bma_data)
}

#' Run a Bayesian model averaging estimation
#'
#' @details Input the BMA data, the variable information data frame
#' and inhereted parameters, which are all the parameters you want to use for the actual
#' estimation inside the 'bms' function. Validate correct input, run the estimation, and
#' return the BMA model without printing any results.
#'
#' @param bma_data [data.frame] The data for BMA. "effect" must be in the first column.
#' @param bma_params [list] Parameters to be used inside the "bms" function. These are:
#' burn, iter, g, mprior, nmodel, mcmc
#' For more info see the "bms" function documentation.
#' @return The bma model
runBMA <- function(bma_data, bma_params) {
  # Input validation
  stopifnot(
    is.data.frame(bma_data),
    !any(is.na(bma_data)), # No missing obs
    all(sapply(bma_data, is.numeric)), # Only numeric obs
    colnames(bma_data[, 1]) == "effect"
  )
  # Get parameters
  all_bma_params <- c(
    list(
      bma_data
    ),
    bma_params
  )
  tryCatch(
    {
      dev.off() # Reset the graphics device
    },
    error = function(e) {
      # message("Could not turn off the null device when plotting the BMA graph") # Does not break anything
    }
  )
  # Actual estimation with inhereted parameters
  set.seed(123) # Cache replicability guarantee
  quiet(
    bma_model <- do.call(bms, all_bma_params)
  )
  return(bma_model)
}

#' Verbose output for the runBMA function
runBMAVerbose <- function(...) {
}

#' Rename the BMA model names to their verbose form using the variable information
#' data frame. Input these two objects (BMA model and variable list DF) and return
#' the modified BMA model.
renameBMAModel <- function(bma_model, input_var_list) {
  # Validate input
  stopifnot(
    class(bma_model) == "bma",
    is.data.frame(input_var_list)
  )
  # Rename the model names
  bma_names <- bma_model$reg.names
  idx <- match(bma_names, input_var_list$var_name)
  bma_names[!is.na(idx)] <- input_var_list$var_name_verbose[na.omit(idx)]
  bma_names[is.na(idx)] <- "Intercept"
  bma_model$reg.names <- bma_names
  return(bma_model)
}


#' Extract results from a Bayesian Model Averaging (BMA) regression
#'
#' @details extractBMAResults is a function that extracts results from a Bayesian Model Averaging (BMA) regression model.
#' The function takes three arguments: bma_model, bma_data, and print_results. bma_model is an object of class bma
#' containing the BMA regression model. bma_data is a data frame containing the data used to fit the BMA model. print_results
#' is a character value indicating the level of result printing desired. The possible values for print_results are "none", "fast",
#' "verbose", and "all".
#'
#' The function first validates the input to ensure that the class of bma_model is "bma", bma_data is a data frame, and
#' print_results is a character value that matches one of the four valid options.
#'
#' The function then extracts the coefficients from bma_model using the coef function. The output is a numeric vector containing
#' the BMA coefficients.
#'
#' The function then prints out coefficient and model statistics based on the value of print_results. If print_results is set to
#' "verbose" or "all", the function prints the coefficients and summary information of bma_model, as well as the top model. If
#' print_results is set to "fast", the function prints only the BMA coefficients. If print_results is set to "all", the function
#' also prints the main BMA plots, which may take some time to generate.
#'
#' Finally, the function plots the correlation matrix of bma_data using corrplot.mixed, and returns the BMA coefficients as a
#' numeric vector.
#'
#' @param bma_model [bma] An object of class bma containing the BMA regression model.
#' @param bma_data [data.frame] A data frame containing the data used to fit the BMA model.
#' @param input_var_list [data.frame] A data frame with the variable information.
#' @param print_results [character] A character value indicating the level of result printing desired.
#'  Can be one of:
#'  * none - print nothing
#'  * fast - print only those results that do not take time to print
#'  * verbose - print all the fast results, plus extra information about the model
#'  * all - print all results, plots included (takes a long time)
#' @param adjustable_theme [logical] If TRUE, modify the plot colors to fit the theme. Defaults to FALSE.
#' @param theme [character] Theme for the two plots.
#' @param export_graphics [logical] If TRUE, export the graphs into the graphics folder. Defaults to TRUE.
#' @param export_path [character] Path to the export folder. Defaults to ./results/graphic.
#' @param graph_scale [numeric] Scale the corrplot graph by this number. Defaults to 1.
#'
#' @return A numeric vector containing only the BMA coefficients.
extractBMAResults <- function(bma_model, bma_data, input_var_list, print_results = "fast", adjustable_theme = F,
                              theme = "blue", export_graphics = T, export_path = "./results/graphic", graph_scale = 1) {
  # Validate the input
  stopifnot(
    class(bma_model) == "bma",
    is.data.frame(bma_data),
    is.data.frame(input_var_list),
    is.character(print_results),
    is.logical(adjustable_theme),
    is.character(theme),
    print_results %in% c("none", "fast", "verbose", "all", "table"),
    is.logical(export_graphics),
    is.character(export_path),
    is.numeric(graph_scale)
  )
  # Rename the variables to verbose form
  bma_model <- renameBMAModel(bma_model, input_var_list)
  # Get verbose names for the bma correlation matirx too
  effect_verbose <- input_var_list$var_name_verbose[match("effect", input_var_list$var_name)]
  bma_matrix_names <- c(effect_verbose, bma_model$reg.names)
  # Extract the coefficients
  bma_coefs <- coef(bma_model, order.by.pip = F, exact = T, include.constant = T)
  # Print out coefficient and model statistics
  if (!print_results == "none") {
    print("Results of the Bayesian Model Averaging:")
  }
  if (print_results %in% c("verbose", "all")) {
    print(bma_model) # Coefficients, summary information
    print(bma_model$topmod[1]) # Topmod
  } else if (print_results == "fast") {
    print(bma_coefs)
  }
  # Create plots for printing/export
  if (any(print_results == "all", export_graphics == TRUE)) {
    # Get the plot theme
    if (adjustable_theme) {
      color_spectrum <- getColors(theme, "bma")
    } else {
      color_spectrum <- c("red", "white", "blue") # Default
    }
    # Main plot
    main_plot_call <- bquote( # Evaluate the color spectrum directly because RRRR
      image(bma_model,
        col = .(color_spectrum), yprop2pip = FALSE, order.by.pip = TRUE,
        do.par = TRUE, do.grid = TRUE, do.axis = TRUE, xlab = "", main = ""
      ) # Takes time
    )
    # Model distribution
    dist_color_spectrum <- color_spectrum[color_spectrum != "white"] # Pop white
    bma_dist_call <- bquote(
      base::plot(bma_model, col = .(dist_color_spectrum))
    )
    # Corrplot
    bma_matrix <- cor(bma_data)
    dimnames(bma_matrix) <- lapply(dimnames(bma_matrix), function(x) {
      bma_matrix_names
    }) # Rename
    bma_col <- colorRampPalette(color_spectrum) # Color palette
    corrplot_mixed_call <- quote( # Simple eval, works for some reason
      corrplot.mixed(bma_matrix,
        lower = "number", upper = "circle",
        lower.col = bma_col(200), upper.col = bma_col(200), tl.pos = c("lt"),
        diag = c("u"), tl.col = "black", tl.srt = 70, tl.cex = 0.55,
        number.cex = 0.5, cl.cex = 0.8, cl.ratio = 0.1
      )
    )
  }
  # Print out plots (takes time)
  if (print_results == "all") {
    print("Printing out Bayesian Model Averaging plots. This may take some time...")
    eval(main_plot_call, envir = environment())
    eval(bma_dist_call, envir = environment())
    eval(corrplot_mixed_call, envir = environment())
  }
  # Return coefficients only
  if (!print_results == "none") {
    cat("\n\n")
  }
  if (export_graphics) {
    # Get the model flagging information
    gprior <- bma_model$gprior.info$gtype
    mprior <- bma_model$mprior.info$origargs$mpmode
    # Paths
    validateFolderExistence(export_path)
    main_path <- paste0(export_path, "/bma_", gprior, "_", mprior, "_results.png")
    dist_path <- paste0(export_path, "/bma_", gprior, "_", mprior, "_dist.png")
    corrplot_path <- paste0(export_path, "/bma_", gprior, "_", mprior, "_corrplot.png")
    # Remove existing plots if they exist
    for (path in list(main_path, dist_path, corrplot_path)) {
      hardRemoveFile(path)
    }
    # Main plot
    png(main_path,
      width = 933 * graph_scale, height = 894 * graph_scale, units = "px",
      res = 70 * graph_scale
    )
    eval(main_plot_call, envir = environment())
    dev.off()
    # Model distribution
    png(dist_path,
      width = 528 * graph_scale, height = 506 * graph_scale, units = "px",
      res = 90 * graph_scale
    )
    eval(bma_dist_call, envir = environment())
    dev.off()
    # Corrplot
    png(corrplot_path,
      width = 700 * graph_scale, height = 669 * graph_scale, units = "px",
      res = 90 * graph_scale
    ) # pointsize for text size
    eval(corrplot_mixed_call, envir = environment())
    dev.off()
  }
  return(bma_coefs)
}

#' Verbose output for the extractBMAResults function
extractBMAResultsVerbose <- function(...) {
  # TODO
}

graphBMAComparison <- function(bma_models, input_var_list, theme = "blue", verbose = T, export_graphics = T,
                               export_path = "./results/graphic", graph_scale = 2) {
  # Rename the BMA models to verbose
  bma_models <- lapply(bma_models, renameBMAModel, input_var_list = input_var_list)
  # Loop through the BMA objects and construct the partial call strings
  bma_model_calls <- list()
  for (i in seq_along(bma_models)) {
    bma_model <- bma_models[[i]]
    # Get the model flagging information
    gprior <- bma_model$gprior.info$gtype
    mprior <- bma_model$mprior.info$origargs$mpmode
    prior_info_verbose <- paste(gprior, "and", mprior)
    # Construct and save the partial call
    model_call <- glue('"{prior_info_verbose}"=bma_models[[{i}]]')
    bma_model_calls <- append(bma_model_calls, model_call)
  }
  # Construct the final call string
  call_str <- paste0("BMS::plotComp(", paste(bma_model_calls, collapse = ", "), ", add.grid=F, cex.xaxis=0.7)")
  graph_call <- parse(text = call_str)
  # Create an environment to evaluate the expression under
  # Plot the plot
  if (verbose) {
    eval(graph_call, envir = environment())
  }
  # Export the plot
  if (export_graphics) {
    # Export the results
    validateFolderExistence(export_path)
    main_path <- paste0(export_path, "/bma_comparison", ".png")
    hardRemoveFile(main_path) # Remove the graph if it exists
    # Save the plot
    png(main_path,
      width = 520 * graph_scale, height = 478 * graph_scale, units = "px",
      res = 90 * graph_scale
    )
    eval(graph_call, envir = environment())
    dev.off()
  }
  return(NULL)
}


#' A helper function (not used in the main analysis) that allows the user to generate
#' a boolean vector for the excel variable info sheet - namely the "bma" column.
#'
#' Serves as a way to extract the list of variables that the optimal BMA formula chooses.
#'
#' @param input_var_list [data.frame] The data frame with variable information.
#' @param bma_formula [formula] Formula of the BMA model.
#' @param verbose [bool] If TRUE, print out the resulting boolean vector into the console.
#' Defaults to TRUE.
#' @return Boolean vector.
getBMAExcelBool <- function(input_var_list, bma_formula, verbose = T) {
  # Input validation
  stopifnot(
    is.data.frame(input_var_list),
    is_formula(bma_formula),
    is.logical(verbose)
  )
  # Get the excel "bma" boolean
  bma_vars <- all.vars(bma_formula)
  bma_bool <- input_var_list$var_name %in% bma_vars
  if (verbose) {
    print(bma_bool)
  }
  invisible(bma_bool)
}
