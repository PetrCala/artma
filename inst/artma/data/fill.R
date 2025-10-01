#' Fill missing degrees of freedom based on t-values and PCCs
#'
#' @note Only use this function for interpolating missing degrees of freedom of PCC-type effects. The formula does not work for other effect types.
#' @param df *\[data.frame\]* The input data frame.
#' @param replace_existing *\[logical\]* Whether to replace existing degrees of freedom. The default is NULL.
#' @param drop_missing *\[logical\]* Whether to drop rows with missing degrees of freedom. The default is NULL.
#' @param drop_negative *\[logical\]* Whether to drop rows with negative degrees of freedom. The default is NULL.
#' @param drop_zero *\[logical\]* Whether to drop rows with zero degrees of freedom. The default is NULL.
#' @return *\[data.frame\]* The modified data frame with updated degrees of freedom.
#' @export
fill_dof_using_pcc <- function(df, replace_existing = NULL, drop_missing = NULL, drop_negative = NULL, drop_zero = NULL) {
  box::use(
    artma / libs / utils[get_verbosity],
    dof_calc = artma / calc / dof
  )

  pcc <- df$effect
  t_values <- df$t_value
  dof <- df$dof

  fillable_rows <- !is.na(t_values) & !is.na(pcc)

  if (!replace_existing) {
    fillable_rows <- fillable_rows & is.na(dof) # Only missing values
  }

  if (sum(fillable_rows) == 0) {
    return(df)
  }
  df[fillable_rows, "dof"] <- dof_calc$calculate_dof(
    t_value = t_values[fillable_rows],
    pcc = pcc[fillable_rows]
  )

  if (get_verbosity() >= 3) {
    cli::cli_inform("Filled {sum(fillable_rows)} missing degrees of freedom.")
  }

  #' A helper function to drop rows based on a condition
  drop_rows <- function(condition, msg) {
    n_rows_to_drop <- sum(condition)
    if (n_rows_to_drop > 0) {
      if (get_verbosity() >= 3) {
        cli::cli_inform("Dropping {n_rows_to_drop} {msg}")
      }
      return(df[!condition, ])
    }
    df
  }

  if (drop_missing) {
    df <- drop_rows(is.na(df$dof), "rows with missing degrees of freedom.")
  }

  if (drop_negative) {
    df <- drop_rows(df$dof < 0, "rows with negative degrees of freedom.")
  }

  if (drop_zero) {
    df <- drop_rows(df$dof == 0, "rows with zero degrees of freedom.")
  }

  return(df)
}
