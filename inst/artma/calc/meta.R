#' @title Calculate t-statistic
#' @description Calculate the t-statistic from effect and SE
#' @param effect *\[numeric\]* The effect
#' @param se *\[numeric\]* The standard error
#' @return *\[numeric\]* The t-statistic
t_stat <- function(effect, se) {
  box::use(
    artma / libs / core / validation[assert],
    artma / libs / core / utils[get_verbosity]
  )
  assert(sum(is.na(effect)) == 0, "The 'effect' column contains missing values")
  assert(sum(is.na(se)) == 0, "The 'se' column contains missing values")

  t_values <- effect / se

  zero_se_rows <- which(se == 0)
  if (length(zero_se_rows) > 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Introducing infinite t-values for {length(zero_se_rows)} rows with zero standard errors")
    }
    t_values[zero_se_rows] <- Inf
  }

  t_values
}


#' @title Calculate the degrees of freedom for the regression
#' @description Calculate the degrees of freedom for the regression
#' @param n_obs *\[numeric\]* The number of observations
#' @param n_predictors *\[numeric\]* The number of predictors
#' @return *\[numeric\]* The degrees of freedom for the regression
reg_dof <- function(n_obs, n_predictors) n_obs - n_predictors


#' @title Calculate the precision of the effect estimates
#' @description Calculate the precision of the effect estimates
#' @param se *\[numeric, optional\]* The standard error. Has to be provided if `reg_dof` is not provided.
#' @param reg_dof *\[numeric, optional\]* The degrees of freedom for the regression. Has to be provided if `se` is not provided.
#' @return *\[numeric\]* The precision of the effect estimates
precision <- function(se = NULL, reg_dof = NULL) {
  box::use(artma / libs / core / utils[get_verbosity])

  precision_type <- getOption("artma.calc.precision_type", "1/SE")
  if (precision_type == "1/SE") {
    zero_se_rows <- which(se == 0)
    if (length(zero_se_rows) > 0) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Introducing infinite precision values for {length(zero_se_rows)} rows with zero standard errors")
      }
    }
    return(1 / se)
  }
  if (precision_type == "DoF") {
    n_negative_dof_rows <- length(which(reg_dof < 0))
    if (n_negative_dof_rows > 0) {
      cli::cli_abort("Negative degrees of freedom found in {n_negative_dof_rows} rows. Aborting precision calculation.")
    }
    return(sqrt(reg_dof))
  }
  cli::cli_abort("Invalid precision type: {precision_type}")
}


box::export(
  t_stat,
  precision,
  reg_dof
)
