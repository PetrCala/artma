box::use(
  artma / data / utils[get_required_colnames],
  artma / libs / utils[get_verbosity]
)

#' @title Detect missing values
#' @description Analyze the data frame for missing values and return a summary.
#' @param df *\[data.frame\]* The data frame to analyze
#' @return *\[list\]* Summary of missing values with columns and counts
#' @keywords internal
detect_missing_values <- function(df) {
  required_cols <- get_required_colnames()
  all_cols <- colnames(df)
  optional_cols <- setdiff(all_cols, required_cols)

  # Count missing values per column
  na_counts <- vapply(all_cols, function(col) sum(is.na(df[[col]])), integer(1))
  names(na_counts) <- all_cols

  # Separate required and optional columns
  required_na <- na_counts[required_cols]
  required_na <- required_na[required_na > 0]

  optional_na <- na_counts[optional_cols]
  optional_na <- optional_na[optional_na > 0]

  # Count rows with any missing values
  rows_with_any_na <- sum(apply(df, 1, function(row) any(is.na(row))))

  list(
    required_cols_with_na = required_na,
    optional_cols_with_na = optional_na,
    total_rows = nrow(df),
    rows_with_any_na = rows_with_any_na,
    has_required_na = length(required_na) > 0,
    has_optional_na = length(optional_na) > 0
  )
}


#' @title Handle missing values with removal strategy
#' @description Remove rows with any missing values (listwise deletion).
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The data frame with complete cases only
#' @keywords internal
handle_na_remove <- function(df) {
  initial_rows <- nrow(df)
  df_complete <- df[stats::complete.cases(df), ]
  removed_rows <- initial_rows - nrow(df_complete)

  if (removed_rows > 0 && get_verbosity() >= 3) {
    cli::cli_alert_success("Removed {.val {removed_rows}} row{?s} with missing values ({.val {round(removed_rows/initial_rows * 100, 1)}%} of data)")
  }

  df_complete
}


#' @title Handle missing values with median imputation
#' @description Replace missing values with the column's median. Works on all numeric columns (both required and optional).
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
handle_na_median <- function(df) {
  imputed_count <- 0

  for (col in colnames(df)) {
    if (is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
      na_indices <- is.na(df[[col]])
      median_val <- stats::median(df[[col]], na.rm = TRUE)

      if (!is.na(median_val)) {
        df[[col]][na_indices] <- median_val
        imputed_count <- imputed_count + sum(na_indices)

        if (get_verbosity() >= 4) {
          cli::cli_alert_info("Imputed {.val {sum(na_indices)}} values in {.field {col}} with median: {.val {round(median_val, 3)}}")
        }
      }
    }
  }

  if (imputed_count > 0 && get_verbosity() >= 3) {
    cli::cli_alert_success("Imputed {.val {imputed_count}} missing value{?s} using median strategy")
  }

  df
}


#' @title Handle missing values with mean imputation
#' @description Replace missing values with the column's mean. Works on all numeric columns (both required and optional).
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
handle_na_mean <- function(df) {
  imputed_count <- 0

  for (col in colnames(df)) {
    if (is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
      na_indices <- is.na(df[[col]])
      mean_val <- mean(df[[col]], na.rm = TRUE)

      if (!is.na(mean_val)) {
        df[[col]][na_indices] <- mean_val
        imputed_count <- imputed_count + sum(na_indices)

        if (get_verbosity() >= 4) {
          cli::cli_alert_info("Imputed {.val {sum(na_indices)}} values in {.field {col}} with mean: {.val {round(mean_val, 3)}}")
        }
      }
    }
  }

  if (imputed_count > 0 && get_verbosity() >= 3) {
    cli::cli_alert_success("Imputed {.val {imputed_count}} missing value{?s} using mean strategy")
  }

  df
}


#' @title Handle missing values with linear interpolation
#' @description Use linear interpolation to fill missing values based on neighboring values. Works on all numeric columns (both required and optional).
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The data frame with interpolated values
#' @keywords internal
handle_na_interpolate <- function(df) {
  imputed_count <- 0

  for (col in colnames(df)) {
    if (is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
      na_indices <- is.na(df[[col]])
      initial_na_count <- sum(na_indices)

      # Use stats::approx for linear interpolation
      valid_indices <- which(!is.na(df[[col]]))

      if (length(valid_indices) >= 2) {
        interpolated <- stats::approx(
          x = valid_indices,
          y = df[[col]][valid_indices],
          xout = seq_len(nrow(df)),
          method = "linear",
          rule = 2 # Use nearest value for extrapolation
        )

        df[[col]] <- interpolated$y
        imputed_count <- imputed_count + initial_na_count

        if (get_verbosity() >= 4) {
          cli::cli_alert_info("Interpolated {.val {initial_na_count}} values in {.field {col}}")
        }
      } else {
        # Fall back to median if not enough points for interpolation
        if (get_verbosity() >= 3) {
          cli::cli_alert_warning("Not enough valid values in {.field {col}} for interpolation; using median instead")
        }
        median_val <- stats::median(df[[col]], na.rm = TRUE)
        if (!is.na(median_val)) {
          df[[col]][na_indices] <- median_val
          imputed_count <- imputed_count + initial_na_count
        }
      }
    }
  }

  if (imputed_count > 0 && get_verbosity() >= 3) {
    cli::cli_alert_success("Imputed {.val {imputed_count}} missing value{?s} using interpolation")
  }

  df
}


#' @title Handle missing values with MICE
#' @description Use Multiple Imputation by Chained Equations to fill missing values. Works on all numeric columns (both required and optional).
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
handle_na_mice <- function(df) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "The {.pkg mice} package is required for multiple imputation",
      "i" = "Install it with: {.code install.packages('mice')}"
    ))
  }

  # Only impute numeric columns that have missing values
  cols_with_na <- colnames(df)[vapply(colnames(df), function(x) {
    is.numeric(df[[x]]) && any(is.na(df[[x]]))
  }, logical(1))]

  if (length(cols_with_na) == 0) {
    return(df)
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Running MICE imputation for {.val {length(cols_with_na)}} column{?s}...")
  }

  # Create a subset with all columns (MICE can handle non-numeric columns as predictors)
  df_subset <- df[, colnames(df), drop = FALSE]

  # Run MICE (suppress output unless verbosity is high)
  mice_obj <- if (get_verbosity() >= 4) {
    mice::mice(df_subset, m = 1, method = "pmm", printFlag = TRUE)
  } else {
    suppressWarnings(suppressMessages(
      mice::mice(df_subset, m = 1, method = "pmm", printFlag = FALSE)
    ))
  }

  # Extract the completed dataset
  df_imputed <- mice::complete(mice_obj, 1)

  # Replace only the imputed columns in the original data frame
  df[, cols_with_na] <- df_imputed[, cols_with_na]

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("MICE imputation completed successfully")
  }

  df
}


#' @title Handle missing values
#' @description Main function to handle missing values according to the selected strategy.
#'
#' This function handles missing values differently for required vs optional columns:
#' - Non-numeric required columns (e.g., study) must be complete and will cause an error if missing
#' - Numeric required columns (e.g., effect, se, n_obs) can be imputed if a non-"stop" strategy is selected
#' - Optional columns are handled according to the selected strategy
#'
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The processed data frame
#' @keywords internal
handle_missing_values <- function(df) {
  box::use(artma / const[CONST])

  # Detect missing values
  na_summary <- detect_missing_values(df)

  # Get the handling strategy
  na_handling <- getOption("artma.data.na_handling", default = "stop")

  # Check if required columns have missing values
  if (na_summary$has_required_na) {
    required_cols_with_na <- names(na_summary$required_cols_with_na)

    # Separate required columns into numeric and non-numeric
    non_numeric_required_with_na <- required_cols_with_na[
      !vapply(required_cols_with_na, function(col) is.numeric(df[[col]]), logical(1))
    ]
    numeric_required_with_na <- setdiff(required_cols_with_na, non_numeric_required_with_na)

    # Non-numeric required columns (like study) cannot be imputed - always error
    if (length(non_numeric_required_with_na) > 0) {
      non_numeric_msg <- paste0(
        non_numeric_required_with_na,
        " (", na_summary$required_cols_with_na[non_numeric_required_with_na], ")",
        collapse = ", "
      )
      cli::cli_abort(c(
        "x" = "Missing values found in non-numeric required columns: {non_numeric_msg}",
        "i" = "Non-numeric required columns (e.g., study) cannot be imputed and must be complete.",
        "i" = "Please clean your data or remove incomplete rows before analysis."
      ))
    }

    # For numeric required columns, check if strategy allows imputation
    if (length(numeric_required_with_na) > 0) {
      if (na_handling == "stop") {
        numeric_msg <- paste0(
          numeric_required_with_na,
          " (", na_summary$required_cols_with_na[numeric_required_with_na], ")",
          collapse = ", "
        )
        cli::cli_abort(c(
          "x" = "Missing values found in required columns: {numeric_msg}",
          "i" = "Current strategy is {.val stop}. Change {.field artma.data.na_handling} to handle missing values automatically.",
          "i" = "Available strategies: {.val remove}, {.val median}, {.val mean}, {.val interpolate}, {.val mice}"
        ))
      }
      # If strategy is not "stop", allow processing to continue (will be handled by imputation functions)
      if (get_verbosity() >= 3) {
        numeric_msg <- paste0(
          numeric_required_with_na,
          " (", na_summary$required_cols_with_na[numeric_required_with_na], ")",
          collapse = ", "
        )
        cli::cli_alert_warning("Missing values detected in numeric required columns: {numeric_msg}. Will apply {.val {na_handling}} strategy.")
      }
    }
  }

  # Check if we need to process missing values
  # We need to process if:
  # 1. There are optional missing values, OR
  # 2. There are numeric required missing values and strategy is not "stop"
  has_numeric_required_na <- if (na_summary$has_required_na) {
    required_cols_with_na <- names(na_summary$required_cols_with_na)
    numeric_required_with_na <- required_cols_with_na[
      vapply(required_cols_with_na, function(col) is.numeric(df[[col]]), logical(1))
    ]
    length(numeric_required_with_na) > 0 && na_handling != "stop"
  } else {
    FALSE
  }

  needs_processing <- na_summary$has_optional_na || has_numeric_required_na

  if (!needs_processing) {
    if (get_verbosity() >= 4) {
      cli::cli_alert_success("No missing values detected that require processing")
    }
    return(df)
  }

  # Report missing values in optional columns
  if (get_verbosity() >= 3 && na_summary$has_optional_na) {
    optional_cols_msg <- paste0(
      names(na_summary$optional_cols_with_na),
      " (", na_summary$optional_cols_with_na, ")",
      collapse = ", "
    )
    cli::cli_alert_warning("Missing values detected in optional columns: {optional_cols_msg}")
  }

  # Apply the selected strategy
  df_processed <- switch(na_handling,
    "stop" = {
      optional_cols_msg <- paste0(
        names(na_summary$optional_cols_with_na),
        " (", na_summary$optional_cols_with_na, ")",
        collapse = ", "
      )
      cli::cli_abort(c(
        "x" = "Missing values found in optional columns: {optional_cols_msg}",
        "i" = "Current strategy is {.val stop}. Change {.field artma.data.na_handling} to handle missing values automatically.",
        "i" = "Available strategies: {.val stop}, {.val remove}, {.val median}, {.val mean}, {.val interpolate}, {.val mice}"
      ))
    },
    "remove" = handle_na_remove(df),
    "median" = handle_na_median(df),
    "mean" = handle_na_mean(df),
    "interpolate" = handle_na_interpolate(df),
    "mice" = handle_na_mice(df),
    {
      cli::cli_abort("Unknown missing value handling strategy: {.val {na_handling}}")
    }
  )

  df_processed
}


box::export(
  detect_missing_values,
  handle_missing_values
)
