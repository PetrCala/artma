box::use(artma / libs / validation[assert, validate])


#' Calculate the PCC variance.
#'
#' @param df *\[data.frame\]* The data frame upon which to calculate the PCC vairance. Should include the columns 'effect', 'sample_size', 'dof'
#' @param offset *\[numeric\]* An offset value to subtract from the degrees of freedom
#'  in case they are missing. Defaults to 0.
#' `vector` A vector of PCC variances.
#' @export
pcc_variance <- function(df, offset = 0) {
  assert(sum(is.na(df$dof)) == 0, "Missing DoF values in the PCC data frame")
  pcc_ <- df$effect

  numerator <- (1 - pcc_^2)^2
  denominator <- df$dof - offset

  variance <- numerator / denominator
  return(variance)
}

#' Calculate random effects
#'
#' @param df *\[data.frame\]* The data frame to calculate the RE with
#' @param effect *\[vector\]* The vector of effects. If not provided, defaults to df$effect.
#' @param se *\[vector\]* The vector of SEs. If not provided, defaults to df$se.
#' @param method *\[character\]* The method to use for the RE calculation. Defaults to "DL". Other common options include "ML" and "REML".
#' @export
re <- function(df, effect = NULL, se = NULL, method = "DL") {
  if (is.null(effect)) effect <- df$effect
  if (is.null(se)) se <- df$se
  validate(length(effect) == nrow(df), length(se) == nrow(df))

  result <- tryCatch(
    {
      meta <- unique(df$meta)
      if (length(meta) != 1) {
        message("Could not calculate RE for multiple meta-analyses")
        return(list(est = NA, t_value = NA))
      }

      re_data_ <- data.frame(yi = effect, sei = se, study = df$study)

      # Sometimes the variances are large
      re_ <- suppressWarnings(
        metafor::rma(yi = yi, sei = sei, data = re_data_, method = method)
      )
      re_est <- re_$beta[1]
      re_se <- re_$se[1]

      re_t_value <- re_est / re_se
      return(list(est = re_est, t_value = re_t_value))
    },
    error = function(e) {
      logger::log_debug(paste("Could not fit the RE model for meta-analysis", meta, ": ", e))
      return(list(est = NA, t_value = NA))
    }
  )

  return(result)
}

#' Calculate UWLS
#'
#' @note Here, the statistics upon which the UWLS is calculated are more variable, thus flexibility is provided when defining the input through the 'effect' and 'se' arguments. To see how this can be leveraged, refer, for example, to the 'uwls3' function, or the 'get_chris_meta_flavours' function.
#'
#' @param df *\[data.frame\]* The data frame on which to run the calculation
#' @param effect *\[vector\]* The vector of effects. If not provided, defaults to df$effect.
#' @param se *\[vector\]* The vector of SEs. If not provided, defaults to df$se.
#' `list` A list with properties "est", "t_value".
#' @export
uwls <- function(df, effect = NULL, se = NULL) {
  if (is.null(effect)) effect <- df$effect
  if (is.null(se)) se <- df$se
  validate(length(effect) == nrow(df), length(se) == nrow(df))

  meta <- unique(df$meta)
  validate(length(meta) == 1)

  t <- effect / se
  precision <- 1 / se

  result <- tryCatch(
    {
      uwls_regression <- stats::lm(t ~ precision - 1)
      summary_uwls <- summary(uwls_regression)
      est <- summary_uwls$coefficients[1, "Estimate"]
      t_value <- summary_uwls$coefficients[1, "t value"]
      return(list(est = est, t_value = t_value))
    },
    error = function(e) {
      logger::log_debug(paste("Could not fit the UWLS model for meta-analysis", meta, ": ", e))
      return(list(est = NA, t_value = NA))
    }
  )

  return(result)
}


#' Calculate UWLS+3
#'
#' @param df *\[data.frame\]* The data frame to calculate the UWLS+3 with
#' `list` A list with properties "est", "t_value".
#' @export
uwls3 <- function(df) {
  meta <- unique(df$meta)
  dof_ <- df$dof
  effect_ <- df$effect
  se_ <- df$se
  t_ <- effect_ / se_

  validate(
    sum(is.na(dof_)) == 0,
    sum(is.na(effect_)) == 0,
    sum(is.na(se_)) == 0
  )

  pcc3 <- t_ / sqrt(t_^2 + dof_ + 3)

  uwls_ <- uwls(df, effect = pcc3, se = se_)
  return(uwls_)
}

#' Calculate the Hunter-Schmidt estimate
#'
#' @param df *\[data.frame\]* The data frame to calculate the UWLS+3 with
#' `list` A list with properties "est", "t_value".
#' @export
hsma <- function(df) {
  meta <- unique(df$meta)
  assert(sum(is.na(df$effect)) == 0, paste("Missing effect values in the PCC data frame for meta-analysis", meta))


  # Safety check
  missing_dof <- sum(is.na(df$dof))
  if (missing_dof > 0) {
    logger::log_debug(paste("Dropping", missing_dof, "missing degrees of freedom for meta-analysis", meta))
    df <- df[!is.na(df$dof), ]
  }

  if (nrow(df) == 0) {
    logger::log_debug(paste("No data to calculate HSMA for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  n_ <- df$dof
  effect <- df$effect
  r_avg <- sum(n_ * effect) / sum(n_)
  sd_sq <- sum(n_ * ((effect - r_avg)^2)) / sum(n_) # SD_r^2 # nolint
  assert(sd_sq >= 0, "Negative SD_r^2")
  se_r <- sqrt(sd_sq) / sqrt(nrow(df)) # SE_r
  t_value <- r_avg / se_r
  return(list(est = r_avg, t_value = t_value))
}

#' Calculate Fisher's z
#'
#' @note For the calculation, all studies should be present in the dataset.
#' @export
fishers_z <- function(df, method = "ML") {
  meta <- unique(df$meta)
  n_ <- df$dof


  # Sometimes the log produces NaNs - handled below
  fishers_z_ <- suppressWarnings(0.5 * log((1 + df$effect) / (1 - df$effect)))
  se_ <- suppressWarnings(1 / sqrt(n_ - 3))

  re_data <- data.frame(effect = fishers_z_, se = se_, meta = df$meta, study = df$study)
  re_data <- re_data[!is.na(fishers_z_) & !is.na(se_), ] # Drop NA rows

  if (nrow(re_data) == 0) {
    logger::log_debug(paste("No data to calculate Fisher's z for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  re_list <- re(df = re_data, method = method)
  re_est <- re_list$est
  re_t_value <- re_list$t_value

  re_z <- (exp(2 * re_est) - 1) / (exp(2 * re_est) + 1)

  return(list(est = re_z, t_value = re_t_value))
}

#' Calculate various summary statistics associated with the PCC data frame
#' @export
pcc_sum_stats <- function(df, log_results = TRUE) {
  meta <- unique(df$meta)
  obs_ <- nrow(df)
  ss_ <- df$sample_size # The sample size to calculate the statistics from

  missing_ss <- is.na(ss_)
  if (sum(missing_ss) > 0) {
    logger::log_debug(paste("Missing sample sizes when calculating PCC summary statistics for meta-analysis", meta, "Filling these using degrees of freedom."))
    ss_[is.na(ss_)] <- df$dof[is.na(ss_)] # Replace NA with DoF
  }
  assert(sum(is.na(ss_)) == 0, "Missing sample sizes in the PCC data frame")

  quantiles <- stats::quantile(ss_, probs = c(0.25, 0.75), na.rm = FALSE)

  # ss_lt ~ sample sizes less than
  get_ss_lt <- function(lt) {
    return(sum(ss_ < lt, na.rm = FALSE) / obs_)
  }

  res <- list(
    n_observations = obs_,
    avg_effect = base::mean(df$effect, na.rm = FALSE),
    avg_ss = base::mean(ss_, na.rm = FALSE),
    median_ss = stats::median(ss_, na.rm = FALSE),
    quantile_1_ss = as.numeric(quantiles[1]),
    quantile_3_ss = as.numeric(quantiles[2]),
    ss_lt_50 = get_ss_lt(50),
    ss_lt_100 = get_ss_lt(100),
    ss_lt_200 = get_ss_lt(200),
    ss_lt_400 = get_ss_lt(400),
    ss_lt_1600 = get_ss_lt(1600),
    ss_lt_3200 = get_ss_lt(3200)
  )

  if (log_results) {
    logger::log_info("PCC analysis summary statistics:")
    logger::log_info(paste("Number of PCC "))
  }
  return(res)
}
