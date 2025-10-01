######################### NON-LINEAR TESTS #########################


#' Extract the four coefficients from linear test in the order
#' - Intercept, Intercept SE
#' Assume a very simplitic form of the non-linear objects, where the coefficients
#' are the the first two positions of the object.
#'
#' @param nonlinear_object Non-linear object from the linear test
#' @param nobs_total [numeric] Number of observations used to estimate the model. Usually the number
#'  of rows in the main data frame.
#' @param nobs_model [numeric] Number of observations that are associated with the particular model.
#'  Optional, defaults to "".
#' @param nonlinear_object Non-linear object from the linear test
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @param pub_bias_present [bool] If T, the method returns publication bias coefs too.
#'  Deafults to F.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @return [vector] - Vector of len 6, with the pub bias coefs, effect coefs, and two nobs information coefs.
extractNonlinearCoefs <- function(nonlinear_object, nobs_total, nobs_model = "",
                                  add_significance_marks = T, pub_bias_present = F, verbose_coefs = T, ...) {
  # Input validation
  stopifnot(
    is.logical(add_significance_marks),
    is.logical(pub_bias_present),
    is.logical(verbose_coefs)
  )
  # Extract coefficients
  effect_coef <- round(as.numeric(nonlinear_object[1, 1]), 3)
  effect_se <- round(as.numeric(nonlinear_object[1, 2]), 3)
  if (add_significance_marks) {
    effect_coef <- add_asterisks(effect_coef, effect_se)
  }
  if (pub_bias_present) {
    pub_coef <- round(as.numeric(nonlinear_object[2, 1]), 3)
    pub_se <- round(as.numeric(nonlinear_object[2, 2]), 3)
    if (add_significance_marks) {
      pub_coef <- add_asterisks(pub_coef, pub_se)
    }
  }
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs) {
    effect_se <- paste0("(", effect_se, ")")
    if (pub_bias_present) {
      pub_se <- paste0("(", pub_se, ")")
    }
  }
  # Group and return quietly
  if (pub_bias_present) {
    nonlin_coefs <- c(pub_coef, pub_se, effect_coef, effect_se, nobs_total, nobs_model) # First two for pub bias
  } else {
    nonlin_coefs <- c("", "", effect_coef, effect_se, nobs_total, nobs_model)
  }
  invisible(nonlin_coefs)
}

###### PUBLICATION BIAS - WAAP (Ioannidis et al., 2017) ######

getWaapResults <- function(data, ...) {
  WLS_FE_avg <- sum(data$effect / data$se) / sum(1 / data$se)
  WAAP_bound <- abs(WLS_FE_avg) / 2.8
  WAAP_data <- data[data$se < WAAP_bound, ] # Only adequatedly powered
  WAAP_reg <- lm(formula = effect ~ -precision, data = WAAP_data)
  WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(data$study_id)))
  nobs_total <- nrow(data)
  nobs_model <- nrow(WAAP_data)
  WAAP_coefs <- extractNonlinearCoefs(WAAP_reg_cluster, nobs_total, nobs_model, ...)
  invisible(WAAP_coefs)
}

###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######


getTop10Results <- function(data, ...) {
  T10_bound <- quantile(data$precision, probs = 0.9) # Setting the 90th quantile bound
  T10_data <- data[data$precision > T10_bound, ] # Only Top10 percent of obs
  T10_reg <- lm(formula = effect ~ -precision, data = T10_data) # Regression using the filtered data
  T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(data$study_id)))
  nobs_total <- nrow(data)
  nobs_model <- nrow(T10_data)
  T10_coefs <- extractNonlinearCoefs(T10_reg_cluster, nobs_total, nobs_model, ...)
  invisible(T10_coefs)
}


###### PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019) #####

#' Compute STEM-based method coefficients from input data
#'
#' This function computes coefficients using the STEM-based method from the \code{stem}
#' package (available at \url{https://github.com/Chishio318/stem-based_method}). The input data
#' should include the necessary columns for the STEM method, and the output will be a numeric
#' vector containing the estimated coefficients.
#'
#' @param data [data.frame] A data frame containing the necessary columns for the STEM-based method
#' @param script_path [character] Full path to the source script.
#' @param representative_sample [character] Representative data sample to choose. One of
#'  "medians", "first", NULL. If set to NULL, use the whole data set. Defaults to "medians".
#' @param print_plot [bool] If TRUE, print out the STEM plot.
#' @param theme [character] Theme for the graphics. Defaults to "blue".
#' @param export_graphics [bool] If TRUE, export the STEM plot.
#' @param export_path [character] Path to the export folder. Deafults to ./results/graphic.
#' @param graph_scale [numeric] Numeric, scale the graph by this number. Defaults to 5.
#' @param legend_pos [character] String specifying where the legend should be placed in the graph.
#'  Defaults to 'topleft'.
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated coefficients for the STEM-based method
#' in the usual format.
#'
#' @import stem_method_master_thesis_cala.R
getStemResults <- function(
    data,
    script_path,
    representative_sample = "medians",
    print_plot = T,
    theme = "blue",
    export_graphics = T,
    export_path = "./results/graphic",
    graph_scale = 5,
    legend_pos = "topleft",
    ...) {
  # Ensure that 'representative_sample' is a valid value
  valid_values <- c("medians", "first", NA)
  if (!representative_sample %in% valid_values) {
    valid_values_str <- paste(valid_values, collapse = ", ")
    stop(glue("'representative_sample' must be one of {valid_values_str}."))
  }

  # Subset the data to the representative sample only
  stem_data <- switch(as.character(representative_sample),
    "medians" = list(
      effect = getMedians(data, "effect"),
      se = getMedians(data, "se")
    ),
    "first" = list(
      effect = getFirst(data, "effect"),
      se = getFirst(data, "se")
    ),
    "NA" = list(
      effect = data$effect,
      se = data$se
    ),
    NULL
  )
  if (!is(stem_data, "list")) {
    stop("The STEM method data selection switch failed to assign a value")
  }

  source(script_path) # github.com/Chishio318/stem-based_method

  stem_param <- c(
    10^(-4), # Tolerance - set level of sufficiently small stem to determine convergence
    10^3 # max_N_count - set maximum number of iteration before termination
  )

  # Estimation
  est_stem <- stem(stem_data$effect, stem_data$se, stem_param)$estimates # Actual esimation
  # Stem plot
  funnel_stem_call <- bquote(
    stem_funnel(stem_data$effect, stem_data$se, est_stem, theme = .(theme), legend_pos = .(legend_pos))
  )
  # Print and export the plot
  if (print_plot) {
    eval(funnel_stem_call)
  }
  if (export_graphics) {
    stopifnot(is.character(export_path), length(export_path) > 0)
    validateFolderExistence(export_path)
    stem_path <- paste0(export_path, "/stem.png")
    hardRemoveFile(stem_path)
    png(stem_path, width = 403 * graph_scale, height = 371 * graph_scale, res = 250)
    eval(funnel_stem_call)
    dev.off()
  }
  # Save results
  nobs_total <- length(stem_data$effect)
  stem_coefs <- extractNonlinearCoefs(est_stem, nobs_total, ...)
  return(stem_coefs)
}


###### PUBLICATION BIAS - FAT-PET hierarchical in R ######

#' Compute hierarchical linear model coefficients from input data
#'
#' This function computes hierarchical linear model coefficients from input data using
#' the \code{rhierLinearModel} function from the \code{bayesm} package. It first organizes
#' the data by study and creates a list of regression data for each study. It then runs the
#' hierarchical linear model using default settings and extracts the estimated coefficients.
#'
#' @param data A data frame containing the necessary columns for the hierarchical linear model
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated coefficients for the hierarchical linear
#' model in the usual format.
#'
#' @import bayesm
getHierResults <- function(data, ...) {
  study_levels_h <- levels(as.factor(data$study_name))
  nreg_h <- length(study_levels_h)
  regdata_h <- NULL
  for (i in 1:nreg_h) {
    filter <- data$study_name == study_levels_h[i] # T/F vector identifying if the observation is from the i-th study
    y <- data$effect[filter] # Effects from the i-th study
    X <- cbind(
      1,
      data$se[filter]
    )
    regdata_h[[i]] <- list(y = y, X = X)
  }
  Data_h <- list(regdata = regdata_h)
  Mcmc_h <- list(R = 6000)

  # Run the model silently
  quiet(
    out_h <- bayesm::rhierLinearModel(
      Data = Data_h,
      Mcmc = Mcmc_h
    ),
  )

  # Save results
  quiet(
    hier_raw_coefs <- summary(out_h$Deltadraw)
  )
  nobs_total <- nrow(data)
  hier_coefs <- extractNonlinearCoefs(hier_raw_coefs, nobs_total, ...)
  invisible(hier_coefs)
}

###### PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019) ######

#' Estimate the Selection Model and extract the coefficients for Effect and its SE
#'  - Source: https://maxkasy.github.io/home/metastudy/
#'
#' This function computes selection model coefficients from input data using
#' the \code{metastudies_estimation} function from the \code{selection_model_master_thesis_cala.R}
#' package. It extracts the estimated effect and publication bias, as well as their
#' standard errors, and returns them as a vector..
#'
#' @param input_data A data frame containing the necessary columns for the selection model
#' @param script_path Full path to the source script.
#' @param cutoffs A numeric vector of cutoff values for computing the selection model
#' coefficients. The default is \code{c(1.960)}, corresponding to a 95% confidence interval.
#' @param symmetric A logical value indicating whether to use the symmetric or asymmetric
#' selection model. The default is \code{FALSE}, indicating the asymmetric model.
#' @param modelmu A character string indicating the type of model to use for the mean
#' effect estimate. The default is \code{"normal"}, corresponding to a normal distribution.
#' Another option is \code{"t"}, corresponding to a t-distribution.
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated effect and publication bias, as well
#' as their standard errors, in the usual format.
#'
#' @import selection_model_master_thesis_cala.R
getSelectionResults <- function(data, script_path, cutoffs = c(1.960),
                                symmetric = F, modelmu = "normal", ...) {
  # Read the source script
  source(script_path)
  # Validate input
  stopifnot(all(cutoffs %in% c(1.645, 1.960, 2.576))) # Cutoffs
  stopifnot(modelmu %in% c("normal", "t")) # Model
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data)))
  # Extract winsorized estimates, standard errors
  sel_X <- data$effect # Effect - Winsorized
  sel_sigma <- data$se # SE - Winsorized
  # Handle argument
  all_params <- list(
    X = sel_X,
    sigma = sel_sigma,
    cutoffs = cutoffs,
    symmetric = symmetric,
    model = modelmu
  )
  estimates <- do.call(
    metastudies_estimation,
    all_params
  )
  # Extract coefficients
  estimates_psi <- estimates$Psihat
  estimates_se <- estimates$SE
  estimates_vec <- c(
    estimates_psi[1], # Effect
    estimates_se[1], # Effect SE
    estimates_psi[2], # Pub Bias
    estimates_se[2] # Pub Bias SE
  )
  estimates_mat <- matrix(estimates_vec, nrow = 2, ncol = 2, byrow = TRUE)
  # Extract the coefficients and return as a vector
  nobs_total <- nrow(data)
  sel_coefs <- extractNonlinearCoefs(estimates_mat, nobs_total, ...)
  return(sel_coefs)
}

###### PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020) ######

#' Estimate the Endogenous Kink model and extract the effect/pub_bias coefficients
#'  - Source: https://osf.io/f6nzb/

#'  @param data [data.frame] The main data frame on which to run the estimation on.
#'    Must contain the columns - "effect", and "se"
#'  @param script_path [character] Path to the source script
#'  @inheritDotParams Parameters for the extractNonlinearCoefs function.
#'
#'  @return endo_kink_coefs [vector] The four desired coefficients, which are:
#'    - Pub bias estimate
#'    - Pub bias standard error
#'    - Mean effect estimate
#'    - Mean effect standard error
#'
#' @import endo_kink_master_thesis_cala.R
#'
#'  Note - The runEndoKink method returns the coefficients in order mean_effect-pub_bias,
#'    this way is just for easier printing into the console, so be mindful of that.
getEndoKinkResults <- function(data, script_path, ...) {
  # Read the source file
  source(script_path)
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data)))
  # Extract winsorized estimates, standard errors
  data <- data[, c("effect", "se")]
  # Run the model estimation and get the four coefficients
  estimates_vec <- runEndoKink(data, verbose = F)
  # Handle output and return verbose coefs
  estimates_mat <- matrix(estimates_vec, nrow = 2, ncol = 2, byrow = TRUE)
  nobs_total <- nrow(data)
  endo_kink_coefs <- extractNonlinearCoefs(estimates_mat, nobs_total, ...)
  return(endo_kink_coefs)
}

###### NON-LINEAR MODELS RESULTS ######

#' Get Non-Linear Tests
#'
#' This function takes in a data frame and returns the results of several non-linear regression methods
#' clustered by study. It first validates that the necessary columns are present in the input data frame.
#' Then, it calls the functions getWaapResults(), getTop10Results(), getStemResults(), getHierResults(),
#' getSelectionResults(), and getEndoKinkResults() to get the coefficients for each method. Finally,
#' it combines the results into a data frame, prints the results to the console, and returns the data
#' frame silently. You may also choose to add significance level asterisks into the final output.
#'
#' @param data The main data frame, onto which all the non-linear methods are then called.
#' @param script_paths List of paths to all source scripts.
#' @param add_significance_marks If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @param theme Theme for the graphics. Defaults to "blue".
#' @param export_graphics If TRUE, export various graphs into the graphics folder.
#' @param export_path Path to the export folder. Defaults to ./results/graphic.
#' @param graph_scale Numeric, scale the graph by this number. Defaults to 5.
#' @param representative_sample [character] Representative data sample to choose. One of
#'  "medians", "first", NULL. If set to NULL, use all data. Defaults to NULL.
#' @param stem_legend_pos [character] String specifying where the legend should be placed in the graph.
#'  Defaults to 'topleft'.
#' @return A data frame containing the results of the non-linear tests, clustered by study.
getNonlinearTests <- function(input_data, script_paths, selection_params = NULL,
                              add_significance_marks = T, theme = "blue",
                              export_graphics = T, export_path = "./results/graphic",
                              graph_scale = 5, stem_representative_sample = "medians",
                              stem_legend_pos = "topleft") {
  # Validate the input

  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    all(required_cols %in% names(input_data)),
    is(script_paths, "list"),
    all(c("stem", "selection", "endo") %in% names(script_paths))
  )
  # Get script_paths
  stem_script_path <- script_paths$stem
  selection_script_path <- script_paths$selection
  endo_script_path <- script_paths$endo
  # Get parameters
  all_selection_params <- c(
    list(
      data = input_data,
      script_path = selection_script_path
    ),
    selection_params,
    list(
      add_significance_marks = add_significance_marks,
      pub_bias_present = T,
      verbose_coefs = T
    )
  )
  all_stem_params <- c(
    list(
      input_data,
      stem_script_path,
      representative_sample = stem_representative_sample,
      print_plot = T,
      theme = theme,
      export_graphics = export_graphics,
      export_path = export_path,
      graph_scale = graph_scale,
      legend_pos = stem_legend_pos,
      add_significance_marks = add_significance_marks,
      pub_bias_present = F,
      verbose_coefs = T
    )
  )
  # Get coefficients
  waap_res <- getWaapResults(input_data, add_significance_marks = add_significance_marks, pub_bias_present = F, verbose_coefs = T)
  top10_res <- getTop10Results(input_data, add_significance_marks = add_significance_marks, pub_bias_present = F, verbose_coefs = T)
  stem_res <- do.call(getStemResults, all_stem_params)
  hier_res <- getHierResults(input_data, add_significance_marks = add_significance_marks, pub_bias_present = T, verbose_coefs = T)
  sel_res <- do.call(getSelectionResults, all_selection_params)
  endo_kink_res <- getEndoKinkResults(input_data, endo_script_path, add_significance_marks = add_significance_marks, pub_bias_present = T, verbose_coefs = T)

  # Combine the results into a data frame
  results <- data.frame(
    waap_df = waap_res,
    top10_df = top10_res,
    stem_df = stem_res,
    hier_df = hier_res,
    sel_df = sel_res,
    endo_kink_df = endo_kink_res
  )

  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)", "Total observations", "Model observations")
  colnames(results) <- c("WAAP", "Top10", "Stem", "Hierarch", "Selection", "Endogenous Kink")
  # Print the results into the console and return
  getNonlinearTestsVerbose(results)
  return(results)
}

#' Verbose output for the getNonlinearTests function
getNonlinearTestsVerbose <- function(res, ...) {
  print("Results of the non-linear tests, clustered by study:")
  print(res)
  cat("\n\n")
}
