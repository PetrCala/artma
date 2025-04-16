#' Modify missing values in a data frame based on changes in other columns
#'
#' @description This function modifies missing values in a data frame based on changes in other columns. If the target column is NA, it is updated with a new value. The new value is a prefix followed by a number that increments each time any of the values in the variable columns changes.
#'
#' @param df *\[data.frame\]* The input data frame.
#' @param target_col *\[character\]* The column to modify if it is NA.
#' @param columns *\[character\]* A vector of column names to detect changes in.
#' @param missing_value_prefix *\[character\]* The prefix to use for missing values.
#'  The default is "missing value".
#' @return *\[data.frame\]* The modified data frame with updated missing values.
#' @example
#' df <- data.frame(
#'  author = c("A", "A", "B", "B", "C"),
#'  year = c(2000, 2000, 2000, 2001, 2002),
#'  study = c("A2000", NA, "B2000", NA, NA)
#' )
#' new_df <- fill_missing_values(df, "study", c("author", "year"), "Missing study")
#' @export
fill_missing_values <- function(df, target_col, columns = NULL, missing_value_prefix = "missing value") {
  box::use(artma / libs / validation[is_char_vector_or_empty])

  cli::cli_inform("Filling missing values for col {.emph {target_col}}...")
  if (!is_char_vector_or_empty(columns) || !is.null(columns)) {
    cli::cli_abort("The columns parameter must be a character vector or empty")
  }
  # No values to modify by
  if (length(columns) == 0) {
    df[[target_col]] <- missing_value_prefix
    return(df)
  }

  # Create the expression for detecting changes
  change_expr <- paste0(
    "(", columns, " != lag(", columns, ", default = first(", columns, ")))"
  )
  change_expr <- paste(change_expr, collapse = " & ")

  # Construct the mutate expression as a string
  change_expr <- paste0("mutate(change = ", change_expr, ")")

  # Evaluate the change expression separately
  df <- eval(parse(text = paste0("df %>% ", change_expr)), envir = environment())
  df$change[is.na(df$change)] <- FALSE

  # Continue with the rest of the pipeline
  df$change <- if (seq_len(nrow(df)) == 1) TRUE else df$change
  df$change_count <- cumsum(df$change)
  na_indices <- is.na(df[[target_col]])
  df[[target_col]][na_indices] <- paste(missing_value_prefix, df$change_count[na_indices])

  df$change <- NULL
  df$change_count <- NULL

  invalid_value <- paste(missing_value_prefix, "NA")
  if (invalid_value %in% df[[target_col]]) {
    cli::cli_abort("The target column contains invalid values. Check the fill function.")
  }

  return(df)
}

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
  box::use(dof_calc = artma / calc / dof)

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
  cli::cli_inform("Filled {sum(fillable_rows)} missing degrees of freedom.")

  #' A helper function to drop rows based on a condition
  drop_rows <- function(condition, msg) {
    n_rows_to_drop <- sum(condition)
    if (n_rows_to_drop > 0) {
      cli::cli_inform("Dropping {n_rows_to_drop} {msg}")
      return(df[!condition, ])
    }
    return(df)
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
