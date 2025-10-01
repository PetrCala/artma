box::use(
  stats[lm, residuals, df.residual, qt, model.matrix],
  base[sqrt, sum, pmax]
)

#' Prepare regression columns for the Endogenous Kink method
#'
#' @param data [data.frame] Two column data.frame with effect sizes and standard errors.
#' @return A data.frame with derived columns required by the method.
prepare_endokink_columns <- function(data) {
  colnames(data) <- c("bs", "sebs")
  enriched <- within(data, {
    ones <- rep(1, length(sebs))
    sebs2 <- sebs^2
    wis <- 1 / sebs2
    bs_sebs <- bs / sebs
    ones_sebs <- 1 / sebs
    bswis <- bs * wis
  })
  enriched[, c("bs", "sebs", "ones", "sebs2", "wis", "bs_sebs", "ones_sebs", "bswis")]
}

#' Fit a linear model without an intercept and return the estimate and standard error
#'
#' @param formula [formula] The regression formula.
#' @param data [data.frame] Regression data.
#' @param coefficient [character] Name of the coefficient to extract.
#' @return A list with elements `estimate`, `std_error`, and `model`.
fit_auxiliary_lm <- function(formula, data, coefficient) {
  model <- lm(formula, data = data)
  summary_table <- summary(model)$coefficients
  list(
    estimate = unname(summary_table[coefficient, "Estimate"]),
    std_error = unname(summary_table[coefficient, "Std. Error"]),
    model = model
  )
}

#' Choose between the PET and PEESE regression results
#'
#' @param pet_estimate [numeric] Point estimate from the PET regression.
#' @param pet_std_error [numeric] Standard error from the PET regression.
#' @param peese_estimate [numeric] Point estimate from the PEESE regression.
#' @param peese_model [stats::lm] Fitted PEESE model.
#' @param pet_model [stats::lm] Fitted PET model.
#' @return A list containing the combined estimate and the associated sum of squared residuals.
select_combined_regression <- function(pet_estimate, pet_std_error, peese_estimate, peese_model, pet_model) {
  m <- nrow(model.matrix(peese_model))
  t_stat <- pet_estimate / pet_std_error
  critical_value <- qt(0.975, m - 2)
  if (abs(t_stat) > critical_value) {
    list(estimate = peese_estimate, residual_sum = sum(residuals(peese_model)^2))
  } else {
    list(estimate = pet_estimate, residual_sum = sum(residuals(pet_model)^2))
  }
}

#' Compute the random effects variance component
#'
#' @param combined_residual_sum [numeric] Sum of squared residuals from the selected regression.
#' @param data [data.frame] Regression data.
#' @param peese_model [stats::lm] The PEESE regression model.
#' @return A list with elements `variance_component` and `standard_deviation`.
compute_variance_component <- function(combined_residual_sum, data, peese_model) {
  m <- nrow(data)
  wis_sum <- sum(data$wis)
  df_model <- df.residual(peese_model)
  sigma_sq <- pmax(0, m * ((combined_residual_sum / (m - df_model - 1)) - 1) / wis_sum)
  list(
    variance_component = sigma_sq,
    standard_deviation = sqrt(sigma_sq)
  )
}

#' Determine the kink cut-off `a1`
#'
#' @param combined_estimate [numeric] Combined regression estimate.
#' @param variance_sd [numeric] Square root of the variance component.
#' @return The cut-off `a1`.
compute_cutoff <- function(combined_estimate, variance_sd) {
  if (combined_estimate > 1.96 * variance_sd) {
    numerator <- (combined_estimate - 1.96 * variance_sd) * (combined_estimate + 1.96 * variance_sd)
    numerator / (2 * 1.96 * combined_estimate)
  } else {
    0
  }
}

#' Fit the final Endogenous Kink regression(s)
#'
#' @param prepared_data [data.frame] Prepared regression data with renamed columns.
#' @param cutoff [numeric] Cut-off value `a1`.
#' @param verbose [logical] Should intermediate results be printed?
#' @return A named numeric vector with the point estimates and standard errors.
final_endokink_fit <- function(prepared_data, cutoff, verbose) {
  sebs_min <- min(prepared_data$sebs)
  sebs_max <- max(prepared_data$sebs)
  if (cutoff > sebs_min && cutoff < sebs_max) {
    prepared_data$sebs_a1 <- if (prepared_data$sebs > cutoff) prepared_data$sebs - cutoff else 0
    prepared_data$pubbias <- prepared_data$sebs_a1 / prepared_data$sebs
    model <- lm(bs ~ 0 + constant + pubbias, data = prepared_data)
    result <- summary(model)$coefficients
    output <- c(result["constant", c("Estimate", "Std. Error")], result["pubbias", c("Estimate", "Std. Error")])
  } else if (cutoff < sebs_min) {
    model <- lm(bs ~ 0 + constant + pub_bias, data = prepared_data)
    result <- summary(model)$coefficients
    output <- c(result["constant", c("Estimate", "Std. Error")], result["pub_bias", c("Estimate", "Std. Error")])
  } else {
    model <- lm(bs ~ 0 + constant, data = prepared_data)
    result <- summary(model)$coefficients
    output <- c(result["constant", c("Estimate", "Std. Error")], c(NA_real_, NA_real_))
  }
  names(output) <- c("constant", "", "pub_bias", "")
  if (isTRUE(verbose)) {
    cli::cli_inform("EK's mean effect estimate (alpha1): {output[[1]]}")
    cli::cli_inform("EK's mean effect standard error: {output[[2]]}")
    cli::cli_inform("EK's publication bias estimate (delta): {output[[3]]}")
    cli::cli_inform("EK's publication bias standard error: {output[[4]]}")
  }
  output
}

#' Run the Endogenous Kink method by Bom & Rachinger (2019)
#'
#' @param data [data.frame] Two column data.frame with effect estimates and standard errors.
#' @param verbose [logical] Should intermediate summaries be printed? Default `TRUE`.
#' @return A numeric vector with the point estimates and standard errors in the same order as the legacy implementation.
run_endogenous_kink <- function(data, verbose = TRUE) {
  stopifnot(
    is.data.frame(data),
    is.logical(verbose),
    ncol(data) == 2,
    all(vapply(data, is.numeric, logical(1)))
  )
  prepared <- prepare_endokink_columns(data)
  pet <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + ones, prepared, "ones_sebs")
  peese <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + sebs, prepared, "ones_sebs")
  combined <- select_combined_regression(pet$estimate, pet$std_error, peese$estimate, peese$model, pet$model)
  variance <- compute_variance_component(combined$residual_sum, prepared, peese$model)
  cutoff <- compute_cutoff(combined$estimate, variance$standard_deviation)
  renamed <- within(prepared, {
    # bs_original <- bs
    bs <- bs_sebs
    constant <- ones_sebs
    pub_bias <- ones
  })
  final_endokink_fit(renamed, cutoff, verbose)
}

box::export(
  run_endogenous_kink,
  prepare_endokink_columns,
  fit_auxiliary_lm,
  select_combined_regression,
  compute_variance_component,
  compute_cutoff,
  final_endokink_fit
)
