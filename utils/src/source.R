######################### RELAXING THE EXOGENEITY ASSUMPTION #########################

#' Extract the four coefficients from an exo test in the order
#' - Pub bias, Pub bias SE, Effect, Effect SE
#' Input a 2 by 2 matrix, where in the first row, you have the effect coefficients,
#'  and in the second row, the pub bias coefficients.
#'
#' @param exo_object [matrix] Object from the exo tests, should be matrix (M(2,2))
#' @param nobs_total [numeric] Number of observations used to estimate the model. Usually the number
#'  of rows in the main data frame.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @param effect_present [bool] If T, the method returns effect coefs. Defaults to T
#' @param pub_bias_present [bool] If T, the method returns publication bias coefs too.
#'  Deafults to T.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @return [vector] The four desired coefficients, which are, in order:
#'    - Pub bias estimate
#'    - Pub bias standard error
#'    - Mean effect estimate
#'    - Mean effect standard error
extractExoCoefs <- function(exo_object, total_obs, add_significance_marks = T,
                            effect_present = T, pub_bias_present = T, verbose_coefs = T) {
  # Validate input
  stopifnot(
    is.numeric(total_obs),
    is.logical(add_significance_marks),
    is.logical(effect_present),
    is.logical(pub_bias_present),
    is.logical(verbose_coefs),
    effect_present || pub_bias_present # At least one
  )
  # Extract coefficients
  effect_coef <- ifelse(effect_present,
    round(as.numeric(exo_object[1, 1]), 3),
    ""
  )
  effect_se <- ifelse(effect_present,
    round(as.numeric(exo_object[1, 2]), 3),
    ""
  )
  pub_coef <- ifelse(pub_bias_present,
    round(as.numeric(exo_object[2, 1]), 3),
    ""
  )
  pub_se <- ifelse(pub_bias_present,
    round(as.numeric(exo_object[2, 2]), 3),
    ""
  )
  # Add significance marks
  if (add_significance_marks) {
    if (effect_present) {
      effect_coef <- add_asterisks(effect_coef, effect_se)
    }
    if (pub_bias_present) {
      pub_coef <- add_asterisks(pub_coef, pub_se)
    }
  }
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs) {
    if (effect_present) {
      effect_se <- paste0("(", effect_se, ")")
    }
    if (pub_bias_present) {
      pub_se <- paste0("(", pub_se, ")")
    }
  }
  # Group and return quietly
  exo_coefs <- c(pub_coef, pub_se, effect_coef, effect_se, total_obs)
  invisible(exo_coefs)
}


#' Identify the best instrument(s) from a set of instruments based on IV regression diagnostics.
#'
#' This function takes in a data frame, a list of potential instruments, and a vector of verbose names for each instrument.
#' The function then runs IV regressions using each of the potential instruments, and returns the instrument(s)
#' with the best performance based on four different diagnostics: R-squared, weak instruments test, Wu-Hausman test,
#' and Sargan test. If multiple instruments are tied for the best performance, all of them will be returned.
#' The function also prints the identified best instrument(s).
#'
#' @param input_data [data.frame] A data frame containing the effect (effect), its standard error (se), study ids, and source
#' data for the instrument(s) (specified as separate columns). It must have the columns "effect", "se", "study_id", and "n_obs".
#' @param instruments [list] A list of potential instruments. Each element of the list should be a vector of numeric values.
#'  Ideally specify as 1/data$n_obs, etc.
#' @param instruments_verbose [vector] A vector of verbose names (strings) for each instrument. It must have the same length
#'  as the number of potential instruments.
#' @return a character vector containing the best instrument(s) identified by the function.
#' @examples
#' data("instrument_data")
#' instruments <- list(instrument_data$instrument1, instrument_data$instrument2)
#' instruments_verbose <- c("Instrument 1", "Instrument 2")
#' findBestInstrument(instrument_data, instruments, instruments_verbose)
findBestInstrument <- function(input_data, instruments, instruments_verbose) {
  # Validity checks
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.list(instruments),
    is.vector(instruments_verbose),
    all(required_cols %in% colnames(input_data))
  )
  # Initialize an empty data frame - each row will be one instrument
  results <- data.frame(
    r_squared = numeric(length(instruments)),
    weak_instruments = numeric(length(instruments)),
    wu_hausman = numeric(length(instruments)),
    sargan = numeric(length(instruments))
  )
  # Run the IV regressions and get diagnostics from each of them
  for (i in seq_along(instruments)) {
    instrument <- instruments[i][[1]] # Unlist
    stopifnot(is.numeric(instrument)) # Amend previous line if this keeps failing - should be only numeric
    instrument_verbose <- instruments_verbose[i]
    input_data$instr_temp <- instrument # Add a column with the instrument values
    iv_formula <- as.formula("effect ~ se | instr_temp")
    model <- ivreg(formula = iv_formula, data = input_data)
    model_summary <- summary(model, vcov = vcovHC(model, cluster = c(input_data$study_id)), diagnostics = T)
    # Extract relevant statistics
    results[i, "r_squared"] <- model_summary$r.squared
    results[i, "weak_instruments"] <- model_summary$diagnostics["Weak instruments", "p-value"]
    results[i, "wu_hausman"] <- model_summary$diagnostics["Wu-Hausman", "p-value"]
    results[i, "sargan"] <- model_summary$diagnostics["Sargan", "p-value"]
  }
  rownames(results) <- instruments_verbose
  # Find the row index with the best performing instrument
  # R-sq
  best_r_squared_idx <- ifelse(any(is.na(results$r_squared)),
    NA,
    which.max(results$r_squared)
  )
  # Weak instr
  best_weak_instruments_idx <- ifelse(any(is.na(results$weak_instruments)),
    NA,
    which.min(results$weak_instruments)
  )
  # Wu Hausman
  best_wu_hausman_idx <- ifelse(any(is.na(results$wu_hausman)),
    NA,
    which.min(results$wu_hausman)
  )
  # Sargan
  best_sargan_idx <- ifelse(any(is.na(results$sargan)),
    NA,
    which.min(results$sargan)
  )
  # Get indexes into a table
  best_instruments_idx <- c(best_r_squared_idx, best_weak_instruments_idx, best_wu_hausman_idx, best_sargan_idx)
  freqs <- table(best_instruments_idx[!is.na(best_instruments_idx)]) # Remove NAs
  stopifnot(length(freqs) > 0) # All NAs
  # Get the most frequent index
  max_freq <- max(freqs)
  max_values <- sapply(names(freqs[freqs == max_freq]), as.numeric) # Numeric index of best performing instrument (or instruments)
  # Get the best instrument(s)
  best_instruments <- rownames(results[max_values, ])
  # Return results - verbose
  if (length(best_instruments > 1)) {
    print(paste0("Identified multiple best instruments:"))
    print(best_instruments)
  } else {
    print(paste0("Identified ", best_instruments, " as the best instrument."))
  }
  return(best_instruments)
}

#' getIVResults function
#'
#' This function takes in data and finds the best instrument for the IV regression of effect against se.
#' It then runs the IV regression and extracts the coefficients. The strength of the function is found
#' in being able to identify the best instrument automatically. The list of instruments is unmodifiable as of now.
#'
#' The four instruments from which the function chooses are:
#' - 1/sqrt(data$n_obs)
#' - 1/data$n_obs
#' - 1/data$n_obs^2
#' - log(data$n_obs)
#'
#' @param data a data frame containing the data for the IV regression
#' @param iv_instrument [character] Instrument to choose in the IV regression. If set to "automatic", determine the best
#' instrument automatically.
#' @inheritDotParams ... additional arguments to be passed to extractExoCoefs
#'
#' @return A list with a numeric vector containing the extracted coefficients from the IV regression,
#' the name of the instrument used, and the Anderson-Rubin F-statistic.
#'
#' @details The function defines a list of instruments to use, and finds the best instrument
#' by running a function called findBestInstrument. If multiple best instruments are identified,
#' the function arbitrarily chooses the first one. The function then runs the IV regression and
#' extracts the coefficients using extractExoCoefs.
#'
#' @examples
#' data <- data.frame(effect = rnorm(10), se = rnorm(10), n_obs = rep(10, 10), study_id = rep(1:10, each = 1))
#' getIVResults(data)
getIVResults <- function(data, iv_instrument = "automatic", ...) {
  # Determine the best instrument automatically
  if (iv_instrument == "automatic") {
    instruments <- list(1 / sqrt(data$n_obs), 1 / data$n_obs, 1 / data$n_obs^2, log(data$n_obs))
    instruments_verbose <- c("1/sqrt(n_obs)", "1/n_obs", "1/n_obs^2", "log(n_obs)")
    # Find out the best instrument
    best_instrument <- findBestInstrument(data, instruments, instruments_verbose)
    # If more best instruments are identified
    if (length(best_instrument) > 1) {
      best_instrument <- best_instrument[1] # Choose the first one arbitrarily
      print(paste("Choosing", best_instrument, "arbitrarily as an instrument for the regression."))
    }
    stopifnot(
      best_instrument %in% instruments_verbose,
      length(best_instrument) == 1 # Should be redundant
    )
    # Get instrument values instead of name
    best_instrument_values <- instruments[match(best_instrument, instruments_verbose)][[1]]
  } else {
    if (!grepl("n_obs", iv_instrument)) {
      stop("The chosen IV instrument must contain the column n_obs.")
    }
    best_instrument <- iv_instrument # Character
    best_instrument_values <- eval(parse(text = gsub("n_obs", "data$n_obs", best_instrument))) # Actual values
  }
  # Run the regression
  data$instr_temp <- best_instrument_values
  iv_formula <- as.formula("effect ~ se | instr_temp")
  model <- ivreg(formula = iv_formula, data = data)
  model_summary <- summary(model, vcov = vcovHC(model, cluster = c(data$study_id)), diagnostics = T)
  # Run the ivmodel regression for fetching of the AR (Anderson-Rubin) test statistic
  model_ar <- ivmodel(Y = data$effect, D = data$se, Z = data$instr_temp)
  fstat <- model_ar$AR$Fstat
  # Get the coefficients
  all_coefs <- model_summary$coefficients
  IV_coefs_vec <- c(
    all_coefs["(Intercept)", "Estimate"], # Effect
    all_coefs["(Intercept)", "Std. Error"], # Effect SE
    all_coefs["se", "Estimate"], # Pub Bias
    all_coefs["se", "Std. Error"] # Pub Bias SE
  )
  iv_coefs_mat <- matrix(IV_coefs_vec, nrow = 2, ncol = 2, byrow = TRUE)
  # Extract the coefficients and return as a vector
  total_obs <- nrow(data)
  iv_coefs_out <- extractExoCoefs(iv_coefs_mat, total_obs, ...)
  # Out list
  iv_out <- list(
    res = iv_coefs_out,
    best_instrument = best_instrument,
    fstat = fstat
  )
  return(iv_out)
}

###### PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) ######


#' getPUniResults - Calculates publication bias test results using the p-uniform method
#'
#' This function calculates publication bias test results using the p-uniform method.
#' It takes in a data frame of the effects with their corresponding standard errors and either uses
#' the Maximum Likelihood (ML) or Moments (P) method to estimate the publication bias.
#'
#' @param data [data.frame] A data frame containing the effects with their corresponding standard errors.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @inheritDotParams Parameters to pass to the main 'puni_star' call
#'
#' @return A vector containing the following four elements:
#' \describe{
#' \item{Test Statistic for the P-uniform publication bias test}{A character string indicatingthe L test
#'  statistic for the P-uniform publication bias test.}
#' \item{P-value for the L test statistic}{A character string indicating the P-value for the L test statistic.}
#' \item{Effect Beyond Bias}{A numeric value indicating the effect beyond bias estimate.}
#' \item{Effect Standard Error}{A character string indicating the standard error of the effect beyond bias estimate.}
#' }
getPUniResults <- function(data, add_significance_marks = T, ...) {
  # Validation
  stopifnot(
    is.data.frame(data)
  )
  # Calculate medians for all studies
  med_yi <- getMedians(data, "effect")
  med_ni <- getMedians(data, "study_size")
  med_ses <- getMedians(data, "se")
  med_sample_sizes <- getMedians(data, "n_obs")
  med_sdi <- med_ses * sqrt(med_sample_sizes) # SD = SE * sqrt(sample_size)
  # Get parameters
  all_params <- c(
    list(
      yi = med_yi,
      vi = med_sdi^2, # Squared sd
      ni = med_ni
    ),
    list(...)
  )
  # Estimation
  quiet(
    est_main <- do.call(
      puni_star,
      all_params
    )
  )
  # Extract and save coefficients - using a custom format for this method
  est_se <- (est_main$ci.ub - est_main$est) / 1.96 # Standard error of the estmiate
  est_effect_verbose <- round(est_main$est, 3) # Effect Beyond Bias
  est_se_verbose <- paste0("(", round(est_se, 3), ")") # Effect Standard Error
  est_pub_test_verbose <- paste0("L = ", round(est_main$L.0, 3)) # Test statistic of p-uni publication bias test
  est_pub_p_val_verbose <- paste0("(p = ", round(est_main$pval.0, 3), ")") # P-value for the L test statistic
  # Add significance marks
  if (add_significance_marks && !is.na(est_effect_verbose)) {
    est_effect_verbose <- add_asterisks(est_effect_verbose, est_se)
  }
  # Return as a vector
  total_obs <- nrow(data)
  p_uni_coefs_out <- c(
    est_pub_test_verbose,
    est_pub_p_val_verbose,
    est_effect_verbose,
    est_se_verbose,
    total_obs
  )
  return(p_uni_coefs_out)
}

#' getExoTests
#'
#' Performs two tests for publication bias and exogeneity in instrumental variable (IV) analyses using clustered data.
#'
#' @param input_data [data.frame] A data frame containing the necessary columns: "effect", "se", "study_id", "study_size", and "precision".
#' @param puni_params [list] Aruments to be used in p-uniform.
#' @param iv_instrument [character] Instrument to choose in the IV regression. If set to "automatic", determine the best
#' instrument automatically.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#'
#' @details This function first validates that the necessary columns are present in the input data frame.
#' If the validation is successful, it performs three tests for publication bias and exogeneity in instrumental variable (IV)
#' analyses using clustered data: the IV test, and the p-Uniform test. The results of the two tests are combined
#' into a data frame, with row names corresponding to the tests and column names corresponding to the test type.
#' The results are then printed into the console and returned invisibly.
getExoTests <- function(input_data, puni_params, iv_instrument = "automatic", add_significance_marks = T) {
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.character(iv_instrument),
    all(required_cols %in% names(input_data))
  )
  # Get arguments
  all_puni_params <- c(
    list(
      data = input_data,
      add_significance_marks = add_significance_marks
    ),
    puni_params
  )
  # Get coefficients
  iv_list <- getIVResults(input_data,
    iv_instrument = iv_instrument, add_significance_marks = add_significance_marks,
    effect_present = T, pub_bias_present = T, verbose_coefs = T
  )
  p_uni_res <- do.call(getPUniResults, all_puni_params)
  # Get results - append F-stat row (extra)
  iv_df <- append(iv_list$res, round(iv_list$fstat, 3))
  p_uni_df <- append(p_uni_res, "")
  # Combine the results into a data frame
  results <- data.frame(
    iv_df = iv_df,
    p_uni_df = p_uni_df
  )
  # Label names
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)", "Total observations", "F-test")
  colnames(results) <- c("IV", "p-Uniform")
  # Print the results into the console and return
  out_list <- list(
    res = results,
    best_instrument = iv_list$best_instrument
  )
  getExoTestsVerbose(out_list)
  return(out_list)
}

#' Verbose output for the getExoTests function
getExoTestsVerbose <- function(result_list, ...) {
  print(paste("Instrument used in the IV regression:", result_list$best_instrument))
  print("Results of the tests relaxing exogeneity, clustered by study:")
  print(result_list$res)
  cat("\n\n")
}
