### Endogenous Kink - Bom & Rachinger (2019)
### Source: https://onlinelibrary.wiley.com/doi/abs/10.1002/jrsm.1352
### Rewritten from STATA code to R - results may be unstable

#' Runs the Endogenous Kink method by Bom & Rachinger (2019) to estimate the mean effect size and the random effects variance component.

#' @param data [data.frame] A data frame with two columns, the first column being the effect data and the second column being the standard error data.
#' @param verbose [bool] A boolean indicating whether to print the results to the console. Default is TRUE.
#' @return A list with the following elements:
#' "b0_ek": the estimated mean effect size by the method
#' "sd0_ek": the standard error of the estimated mean effect size
#' "b1_ek": the estimated publication bias coefficient by the Endo-Kink method, if applicable
#' "sd1_ek": the standard error of the estimated publication bias coefficient, if applicable
runEndoKink <- function(data, verbose = T) {
  # Input validation
  stopifnot(
    is.data.frame(data), # Only data frame
    is.logical(verbose), # Only boolean
    ncol(data) == 2, # Effect data, Standard error data
    sapply(data, is.numeric) # Only numeric input
  )
  # Rename source data
  colnames(data) <- c("bs", "sebs")

  # Create new variables
  data$ones <- 1
  M <- nrow(data)
  sebs_min <- min(data$sebs)
  sebs_max <- max(data$sebs)
  data$sebs2 <- data$sebs^2
  data$wis <- data$ones / data$sebs2
  data$bs_sebs <- data$bs / data$sebs
  data$ones_sebs <- data$ones / data$sebs
  data$bswis <- data$bs * data$wis
  wis_sum <- sum(data$wis) # Redundant

  # FAT-PET
  fat_pet <- lm(bs_sebs ~ 0 + ones_sebs + ones, data = data) # No constant
  # Auxiliary
  fat_pet_est <- coeftest(fat_pet)["ones_sebs", "Estimate"] # Fat pet ones_sebs estimate
  fat_pet_se <- coeftest(fat_pet)["ones_sebs", "Std. Error"] # Fat pet ones_sebs standard error
  # End auxiliary
  pet <- fat_pet_est
  t1_linreg <- fat_pet_est / fat_pet_se
  b_lin <- fat_pet_est
  Q1_lin <- sum(resid(fat_pet)^2)
  abs_t1_linreg <- abs(t1_linreg)

  # PEESE
  peese_model <- lm(bs_sebs ~ 0 + ones_sebs + sebs, data = data) # No constant
  # Auxiliary
  peese_est <- coeftest(peese_model)["ones_sebs", "Estimate"]
  # End auxiliary
  peese <- peese_est
  b_sq <- peese_est
  Q1_sq <- sum(resid(peese_model)^2) # Sum of squared residuals

  # FAT-PET-PEESE
  if (abs_t1_linreg > qt(0.975, M - 2)) {
    combreg <- b_sq
    Q1 <- Q1_sq
  } else {
    combreg <- b_lin
    Q1 <- Q1_lin
  }

  # Estimation of random effects variance component
  df_m <- df.residual(peese_model) # DoF from the last regression (peese-model)
  sigh2hat <- max(0, M * ((Q1 / (M - df_m - 1)) - 1) / wis_sum)
  sighhat <- sqrt(sigh2hat)

  # Cutoff value for EK
  if (combreg > 1.96 * sighhat) {
    a1 <- (combreg - 1.96 * sighhat) * (combreg + 1.96 * sighhat) / (2 * 1.96 * combreg)
  } else {
    a1 <- 0
  }

  # Rename variables - messy source code, kept to the original
  names(data)[names(data) == "bs"] <- "bs_original"
  names(data)[names(data) == "bs_sebs"] <- "bs"
  names(data)[names(data) == "ones_sebs"] <- "constant"
  names(data)[names(data) == "ones"] <- "pub_bias"

  # Regressions and coefficient extraction in various scenarios
  if (a1 > sebs_min & a1 < sebs_max) {
    data$sebs_a1 <- ifelse(data$sebs > a1, data$sebs - a1, 0)
    data$pubbias <- data$sebs_a1 / data$sebs
    ek_regression <- lm(bs ~ 0 + constant + pubbias, data = data)
    b0_ek <- coef(ek_regression)[1]
    b1_ek <- coef(ek_regression)[2]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    sd1_ek <- summary(ek_regression)$coefficients[2, 2]
  } else if (a1 < sebs_min) {
    ek_regression <- lm(bs ~ 0 + constant + pub_bias, data = data)
    b0_ek <- coef(ek_regression)[1]
    b1_ek <- coef(ek_regression)[2]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    sd1_ek <- summary(ek_regression)$coefficients[2, 2]
  } else if (a1 > sebs_max) {
    ek_regression <- lm(bs ~ 0 + constant, data = data)
    b0_ek <- coef(ek_regression)[1]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    b1_ek <- NA
    sd1_ek <- NA
  }
  # Print results to console if desired
  if (verbose) {
    cat("EK's mean effect estimate (alpha1) and standard error:")
    cat(b0_ek) # Mean effect estimate
    cat(sd0_ek) # Mean effect standard error
    cat("EK's publication bias estimate (delta) and standard error:")
    cat(b1_ek) # Pub bias estimate
    cat(sd1_ek) # Pub bias standard error
  }
  # Return the four coefficients
  return(c(b0_ek, sd0_ek, b1_ek, sd1_ek))
}
