######################### LINEAR TESTS #########################

#' Add significance marks (asterisks) to a coefficient. Input the coefficient and its
#' standard error and return that coefficient with the asterisks. Character is always returned,
#' although the input must be numeric.
#'
#' @param coef [numeric] Coefficient.
#' @param se [numeric] Its standard error.
#'
#' @return [character] The coefficient with asterisks. Returned as character.
add_asterisks <- function(coef, se) { # Switch does not really work here as far as I know
  stopifnot(
    is.numeric(coef),
    is.numeric(se)
  )
  # NA values or 0 values
  if (any(
    is.na(coef),
    is.na(se),
    coef == 0,
    se == 0
  )) {
    return(coef)
  }
  tvalue <- coef / se
  if (tvalue > 2.58) {
    asterisks <- "***"
  } else if (tvalue > 1.96) {
    asterisks <- "**"
  } else if (tvalue > 1.645) {
    asterisks <- "*"
  } else {
    asterisks <- ""
  }
  new_value <- paste0(as.character(coef), asterisks)
  return(new_value)
}

#' Extract the four coefficients from linear test in the order
#' - Intercept, Intercept SE, Slope, Slope SE
#'
#' @param coeftest_object Coeftest object from the linear test
#' @param boot_ci_list [list|NA] A list containing the information about the bounds of the wild bootstrap
#'  confidence interval. The list is nested with the first level indicating the effect type (se, constant),
#'  and the second containing the confidence interval bounds info. If the method does not use bootstrapped
#'  CI, set to NA.
#' @param nobs_total [numeric] Number of observations used to estimate the model. Usually the number
#'  of rows in the main data frame.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @return [vector] - Vector of len 4, with the coefficients
extractLinearCoefs <- function(coeftest_object, boot_ci_list, nobs_total, add_significance_marks = T, verbose_coefs = T) {
  # Check validity of the coeftest object
  stopifnot(
    is.numeric(nobs_total),
    is.logical(add_significance_marks),
    is.logical(verbose_coefs),
    nrow(coeftest_object) == 2,
    ncol(coeftest_object) == 4,
    colnames(coeftest_object)[1] == "Estimate",
    colnames(coeftest_object)[2] == "Std. Error"
  )
  # Handle bootstrapped CI input
  boot_ci_is_na <- is.null(boot_ci_list) || all(is.na(boot_ci_list)) # There is no bootsrapping in this method
  if (boot_ci_is_na) {
    # The method does not use bootstrap CI
    pub_bias_boot_ci <- ""
    effect_boot_ci <- ""
  } else {
    # The method uses bootstrap CI - check the input validity
    if (!all(c("pub_bias", "effect") %in% names(boot_ci_list))) {
      stop("The bootstrap CI must be a list that contains the information about the publication bias (se) and the effect (constant).")
    }
    if (!all(c("upper_bound", "lower_bound") %in% names(boot_ci_list$pub_bias))) {
      stop("The publication bias bootstrap CI does not contain info about the CI bounds.")
    }
    if (!all(c("upper_bound", "lower_bound") %in% names(boot_ci_list$effect))) {
      stop("The effect bootstrap CI does not contain info about the CI bounds.")
    }
  }
  # Extract coefficients
  pub_bias_coef <- round(coeftest_object[2, "Estimate"], 3)
  pub_bias_se <- round(coeftest_object[2, "Std. Error"], 3)
  effect_coef <- round(coeftest_object[1, "Estimate"], 3)
  effect_se <- round(coeftest_object[1, "Std. Error"], 3)
  if (add_significance_marks) {
    pub_bias_coef <- add_asterisks(pub_bias_coef, pub_bias_se)
    effect_coef <- add_asterisks(effect_coef, effect_se)
  }
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs) {
    pub_bias_se <- paste0("(", pub_bias_se, ")")
    effect_se <- paste0("(", effect_se, ")")
  }

  # Round elements of a nested list (in case of bootstrapping-method)
  if (!boot_ci_is_na) {
    boot_ci_list <- rapply(boot_ci_list, function(x) round(x, 3), how = "replace")
    # Extract the bootstrap CI information (always verbose)
    pub_bias_boot_ci <- paste0("[", boot_ci_list$pub_bias$lower_bound, ", ", boot_ci_list$pub_bias$upper_bound, "]")
    effect_boot_ci <- paste0("[", boot_ci_list$effect$lower_bound, ", ", boot_ci_list$effect$upper_bound, "]")
  } else {
    pub_bias_boot_ci <- ""
    effect_boot_ci <- ""
  }

  # Group and return quietly
  lin_coefs <- c(
    pub_bias_coef, pub_bias_se, pub_bias_boot_ci,
    effect_coef, effect_se, effect_boot_ci,
    nobs_total
  )
  invisible(lin_coefs)
}

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

### Helper functions

#' getLinearModelFitFunction
#'
#' Fetch a fit function for one of the several available linear methods.
#' Return this function to be called in other methods.
#'
#' @param model_type [character] Name of the linear method to fetch the fit function for.
#' Should be one of "ols", "fe", "be", "re", "ols_w_study", "ols_w_precision".
#'
#' @returns [function] The fit function for the given type of linear method.
getLinearModelFitFunction <- function(model_type) {
  fit_model_function <- switch(model_type,
    "ols" = function(lm_data) lm(formula = effect ~ se, data = lm_data),
    "fe" = function(lm_data) plm(formula = effect ~ se, model = "within", index = "study_id", data = lm_data),
    "be" = function(lm_data) plm(formula = effect ~ se, model = "between", index = "study_id", data = lm_data),
    "re" = function(lm_data) plm(effect ~ se, model = "random", index = "study_id", data = lm_data),
    "ols_w_study" = function(lm_data) lm(formula = effect ~ se, data = lm_data, weight = (lm_data$study_size * lm_data$study_size)),
    "ols_w_precision" = function(lm_data) lm(formula = effect ~ se, data = lm_data, weight = c(lm_data$precision * lm_data$precision)),
    stop("Invalid model type")
  )

  return(fit_model_function)
}

#' getLinearModelBootCI
#'
#' Compute wild bootstrap confidence intervals for a linear method based on its name
#' and a data frame. Return the result as a list containing the confidence intervals
#' for the slope and the intercept.
#'
#' @param model_type [character] Type of the linear model to estimate. Should be one of
#'  "ols", "fe", "be", "re", "ols_w_study", "ols_w_precision".
#' @param data [data.frame] A data frame to run the estimation on.
#' @param R [integer] The number of bootstrap replications to perform. This parameter determines
#' how many times the bootstrap resampling is repeated. Default is 100.
#'
#' @returns [list] A nested list that contains two named elements - pub_bias, and effect.
#'  Each contains information about the wild bootstrap confidence interval in terms
#'  of its bounds - "upper_bound" and "lower_bound".
#'
#' @examples
#' ols_boot_ci_list <- getLinearModelBootCI("ols", some_data_frame, R = 500)
#' print(ols_boot_ci_list$pub_bias$lower_bound) # 5
#' print(ols_boot_ci_list$pub_bias$upper_bound) # *
getLinearModelBootCI <- function(model_type, data, R = 100) {
  fit_model <- getLinearModelFitFunction(model_type)

  fit_model_intercept <- function(boot_data) {
    fit <- fit_model(boot_data)
    return(coef(fit)[1]) # Return the intercept
  }
  # Function to fit the model and return the slope
  fit_model_slope <- function(boot_data) {
    fit <- fit_model(boot_data)
    return(coef(fit)[2]) # Return the slope (coefficient of x)
  }

  # Get the wild bootstrap confidence intervals
  boot_ci_intercept <- getBootstrappedCI(data, fit_model_intercept, R = R, model_type = model_type)
  boot_ci_slope <- getBootstrappedCI(data, fit_model_slope, R = R, model_type = model_type)

  boot_ci_list <- list(
    pub_bias = boot_ci_slope,
    effect = boot_ci_intercept
  )

  return(boot_ci_list)
}

#' Run all the linear tests on data, and return a matrix of results.
#' These tests are ran: OLS, FE, BE, RE, Weighted OLS (by study size),
#'  Weighted OLS (by precision). You may also choose to add significance
#'  level asterisks to the final output.
#'
#' @param data [data.frame] Input data
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @param R [integer] The number of bootstrap replications to perform. This parameter determines
#' how many times the bootstrap resampling is repeated. Default is 100.
#' @param verbose [logical] If TRUE, print out verbose output about the tests run, including a progress bar.
#'  Default is T.
getLinearTests <- function(data, add_significance_marks = T, R = 100, verbose = T) {
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data)))
  total_obs <- nrow(data)
  ### OLS
  getOLS <- function() {
    fit_ols <- getLinearModelFitFunction("ols")
    ols <- fit_ols(data)
    ols_res <- coeftest(ols, vcov = vcovHC(ols, type = "HC0", cluster = c(data$study_id)))
    ols_boot_ci_list <- getLinearModelBootCI("ols", data, R = R)
    ols_coefs <- extractLinearCoefs(ols_res, ols_boot_ci_list, total_obs, add_significance_marks)
    return(ols_coefs)
  }
  ### Fixed-effects
  getFE <- function() {
    fit_fe <- getLinearModelFitFunction("fe")
    fe <- fit_fe(data)
    fe_res <- coeftest(fe, vcov = vcov(fe, type = "fixed", cluster = c(data$study_id)))
    # Calculate the intercept and extract the coefficients
    fe_intercept <- within_intercept(fe, vcov = function(fe) {
      vcov(fe, type = "fixed", cluster = c(data$study_id))
    })
    fe_effect_coef <- fe_intercept[[1]] # Coefficient
    fe_effect_se <- attr(fe_intercept, "se") # SE
    # Bind together the values into the existing coeftest object
    names(fe_res) <- c("Estimate", "Str. Error")
    new_row <- c(fe_effect_coef, fe_effect_se)
    new_fe_res <- rbind(new_row, fe_res)
    # Extract the coefficients - not 100% confident whether to use Bootstrap here (set to NA otherwise)
    fe_boot_ci_list <- getLinearModelBootCI("fe", data, R = R)
    # fe_boot_ci_list <- NA
    fe_coefs <- extractLinearCoefs(new_fe_res, fe_boot_ci_list, total_obs, add_significance_marks)
    return(fe_coefs)
  }
  ### Between effects
  getBE <- function() {
    fit_be <- getLinearModelFitFunction("be")
    be <- fit_be(data)
    be_res <- coeftest(be, vcov = vcov(be, type = "fixed", cluster = c(data$study_id)))
    be_boot_ci_list <- NA # Do not use wild bootstrap for BE
    be_coefs <- extractLinearCoefs(be_res, be_boot_ci_list, total_obs, add_significance_marks)
    return(be_coefs)
  }
  ### Random Effects
  getRE <- function() {
    fit_re <- getLinearModelFitFunction("re")
    re <- fit_re(data)
    re_res <- coeftest(re, vcov = vcov(re, type = "fixed", cluster = c(data$study_id)))
    re_boot_ci_list <- getLinearModelBootCI("re", data, R = R)
    re_coefs <- extractLinearCoefs(re_res, re_boot_ci_list, total_obs, add_significance_marks)
    return(re_coefs)
  }
  ### Weighted by number of observations per study
  getOLSWStudy <- function() {
    fit_ols_w_study <- getLinearModelFitFunction("ols_w_study")
    ols_w_study <- fit_ols_w_study(data)
    ols_w_study_res <- coeftest(ols_w_study, vcov = vcovHC(ols_w_study, type = "HC0", cluster = c(data$study_id)))
    ols_w_study_boot_ci_list <- getLinearModelBootCI("ols_w_study", data, R = R)
    ols_w_study_coefs <- extractLinearCoefs(ols_w_study_res, ols_w_study_boot_ci_list, total_obs, add_significance_marks)
    return(ols_w_study_coefs)
  }
  ### Weighted by precision
  getOLSWPrecision <- function() {
    fit_ols_w_precision <- getLinearModelFitFunction("ols_w_precision")
    ols_w_precision <- fit_ols_w_precision(data)
    ols_w_precision_res <- coeftest(ols_w_precision, vcov = vcovHC(ols_w_precision, type = "HC0", cluster = c(data$study_id)))
    ols_w_precision_boot_ci_list <- getLinearModelBootCI("ols_w_precision", data, R = R)
    ols_w_precision_coefs <- extractLinearCoefs(ols_w_precision_res, ols_w_precision_boot_ci_list, total_obs, add_significance_marks)
    return(ols_w_precision_coefs)
  }
  linear_methods_list <- list(
    "ols" = getOLS,
    "fe" = getFE,
    "be" = getBE,
    "re" = getRE,
    "ols_w_study" = getOLSWStudy,
    "ols_w_precision" = getOLSWPrecision
  )

  # Call a single method function
  run_linear_method <- function(fit_function, method_name) {
    res <- fit_function()

    if (verbose) {
      message <- paste0("Running method: ", method_name)
      cat(sprintf("%-100s", message)) # Add enough whitespace to make sure the whole line is cleared
      flush.console()

      cat("\r")
    }

    return(res)
  }

  # Call each method in a progress bar lapply loop
  res_list <- pbapply::pblapply(names(linear_methods_list), function(method_name) {
    run_linear_method(linear_methods_list[[method_name]], method_name) # No args means easier use but environment reliancy
  })
  names(res_list) <- names(linear_methods_list) # Restore the original names

  # Combine the results into a data frame
  results <- data.frame(
    OLS = res_list$ols,
    FE = res_list$fe,
    BE = res_list$be,
    RE = res_list$re,
    OLS_weighted_study = res_list$ols_w_study,
    OLS_weighted_precision = res_list$ols_w_precision
  )
  rownames(results) <- c(
    "Publication Bias", "(Standard Error)", "Bootstrapped CI (PB)",
    "Effect Beyond Bias", "(Constant)", "Bootstrapped CI (EBB)",
    "Total observations"
  )
  colnames(results) <- c(
    "OLS", "Fixed Effects", "Between Effects",
    "Random Effects", "Study weighted OLS", "Precision weighted OLS"
  )
  # Print the results into the console and return
  getLinearTestsVerbose(results)
  return(results)
}

#' Verbose output for the getLinearTests function
getLinearTestsVerbose <- function(res, ...) {
  print("Results of the linear tests, clustered by study:")
  print(res)
  cat("\n\n")
}
