box::use(
  artma / data / utils[get_required_colnames, get_winsorization_recomputed_cols],
  artma / libs / core / utils[get_verbosity]
)

#' @title Format columns with NA counts
#' @description Format a named count vector as "col (n), col (n)" for messages.
#' @param counts *\[integer\]* Named vector of NA counts per column
#' @return *\[character\]* Single formatted string
#' @keywords internal
format_cols_with_counts <- function(counts) {
  paste0(names(counts), " (", counts, ")", collapse = ", ")
}


#' @title Label NA counts with source column names
#' @description Rename a per-column NA count vector so messages show the user's
#'   source column name next to the standardized one. Falls back to the
#'   standardized name when no mapping exists (the column store is sparse).
#' @param counts *\[integer\]* Named vector of NA counts, keyed by standardized column names
#' @return *\[integer\]* The same counts with display-ready names
#' @keywords internal
label_with_source_names <- function(counts) {
  box::use(artma / data / utils[get_colnames_map])
  map <- get_colnames_map()
  names(counts) <- vapply(
    names(counts),
    function(std) {
      src <- map[[std]]
      if (is.null(src) || identical(src, std)) std else sprintf("%s (source: %s)", std, src)
    },
    character(1)
  )
  counts
}


#' @title Detect missing values
#' @description Analyze the data frame for missing values and return a summary.
#' @param df *\[data.frame\]* The data frame to analyze
#' @param ignore_cols *\[character, optional\]* Columns left out of the scan,
#'   e.g. columns the compute phase is about to recalculate anyway
#' @return *\[list\]* Summary of missing values with columns and counts
#' @keywords internal
detect_missing_values <- function(df, ignore_cols = character(0)) {
  all_cols <- setdiff(colnames(df), ignore_cols)
  # A required column the frame does not carry (possible since methods declare
  # their own requirements, see #400) must not enter the NA scan: indexing
  # na_counts by an absent name would inject a phantom NA-named entry that
  # aborts the run with an unnamed "missing values" error.
  required_cols <- intersect(get_required_colnames(), all_cols)
  optional_cols <- setdiff(all_cols, required_cols)

  # Count missing values per column
  na_counts <- vapply(all_cols, function(col) sum(is.na(df[[col]])), integer(1))

  # Separate required and optional columns
  required_na <- na_counts[required_cols]
  required_na <- required_na[required_na > 0]

  optional_na <- na_counts[optional_cols]
  optional_na <- optional_na[optional_na > 0]

  # Count rows with any missing values
  rows_with_any_na <- sum(rowSums(is.na(df[, all_cols, drop = FALSE])) > 0)

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
#' @param exclude_cols *\[character, optional\]* Columns whose missing values
#'   do not cause row removal
#' @return *\[data.frame\]* The data frame with complete cases only
#' @keywords internal
handle_na_remove <- function(df, exclude_cols = character(0)) {
  initial_rows <- nrow(df)
  checked_cols <- setdiff(colnames(df), exclude_cols)
  df_complete <- df[stats::complete.cases(df[, checked_cols, drop = FALSE]), ]
  removed_rows <- initial_rows - nrow(df_complete)

  if (removed_rows > 0 && get_verbosity() >= 3) {
    cli::cli_alert_success("Removed {.val {removed_rows}} row{?s} with missing values ({.val {round(removed_rows/initial_rows * 100, 1)}%} of data)")
  }

  df_complete
}


#' @title Impute missing values with a column summary statistic
#' @description Replace missing values in every numeric column with `stat_fun`
#' of that column. Works on all numeric columns (both required and optional).
#' @param df *\[data.frame\]* The data frame to process
#' @param stat_fun *\[function\]* Summary function applied with `na.rm = TRUE`
#' @param strategy *\[character\]* Strategy label used in messages
#' @param exclude_cols *\[character, optional\]* Columns to leave unimputed
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
impute_with_column_stat <- function(df, stat_fun, strategy, exclude_cols = character(0)) {
  imputed_count <- 0

  for (col in setdiff(colnames(df), exclude_cols)) {
    if (is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
      na_indices <- is.na(df[[col]])
      stat_val <- stat_fun(df[[col]], na.rm = TRUE)

      if (!is.na(stat_val)) {
        df[[col]][na_indices] <- stat_val
        imputed_count <- imputed_count + sum(na_indices)

        if (get_verbosity() >= 4) {
          cli::cli_alert_info("Imputed {.val {sum(na_indices)}} values in {.field {col}} with {strategy}: {.val {round(stat_val, 3)}}")
        }
      }
    }
  }

  if (imputed_count > 0 && get_verbosity() >= 3) {
    cli::cli_alert_success("Imputed {.val {imputed_count}} missing value{?s} using {strategy} strategy")
  }

  df
}


#' @title Handle missing values with median imputation
#' @param df *\[data.frame\]* The data frame to process
#' @param exclude_cols *\[character, optional\]* Columns to leave unimputed
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
handle_na_median <- function(df, exclude_cols = character(0)) {
  impute_with_column_stat(df, stats::median, "median", exclude_cols = exclude_cols)
}


#' @title Handle missing values with mean imputation
#' @param df *\[data.frame\]* The data frame to process
#' @param exclude_cols *\[character, optional\]* Columns to leave unimputed
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
handle_na_mean <- function(df, exclude_cols = character(0)) {
  impute_with_column_stat(df, mean, "mean", exclude_cols = exclude_cols)
}


#' @title Handle missing values with linear interpolation
#' @description Use linear interpolation to fill missing values based on neighboring values. Works on all numeric columns (both required and optional).
#' @param df *\[data.frame\]* The data frame to process
#' @param exclude_cols *\[character, optional\]* Columns to leave unimputed
#' @return *\[data.frame\]* The data frame with interpolated values
#' @keywords internal
handle_na_interpolate <- function(df, exclude_cols = character(0)) {
  imputed_count <- 0

  for (col in setdiff(colnames(df), exclude_cols)) {
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
#' @description Use Multiple Imputation by Chained Equations to fill missing values.
#' Includes automatic detection and exclusion of problematic columns (dummy groups,
#' near-zero variance) to prevent singularity errors.
#' @param df *\[data.frame\]* The data frame to process
#' @param exclude_cols *\[character, optional\]* Columns to leave unimputed and
#'   keep out of the MICE predictor set (e.g. mostly-missing columns caught by
#'   the missingness-ratio guard)
#' @return *\[data.frame\]* The data frame with imputed values
#' @keywords internal
handle_na_mice <- function(df, exclude_cols = character(0)) {
  box::use(artma / data / profile[detect_dummy_groups])

  # suppressMessages: this is the first `mice::` reference in the session on
  # a fresh load, so requireNamespace() here (not just the later mice::mice()
  # call) is what triggers mice's own Imports (car, lme4) to load, printing a
  # raw "Registered S3 method overwritten" line with no user-actionable
  # content.
  if (!suppressMessages(requireNamespace("mice", quietly = TRUE))) {
    cli::cli_abort(c(
      "x" = "The {.pkg mice} package is required for multiple imputation",
      "i" = "Install it with: {.code install.packages('mice')}"
    ))
  }

  # Identify numeric columns with missing values

  cols_with_na <- colnames(df)[vapply(colnames(df), function(x) {
    is.numeric(df[[x]]) && any(is.na(df[[x]]))
  }, logical(1))]

  if (length(cols_with_na) == 0) {
    return(df)
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Running MICE imputation for {.val {length(cols_with_na)}} column{?s}...")
  }

  # --- Pre-MICE validation: detect problematic columns ---

  df_for_mice <- df
  excluded_cols <- exclude_cols
  dummy_groups <- data.frame()

  # 1. Detect dummy groups (e.g., gender_male/gender_female) - exclude non-reference

  dummy_groups <- detect_dummy_groups(colnames(df), df)
  if (nrow(dummy_groups) > 0) {
    non_ref_dummies <- dummy_groups$var_name[!dummy_groups$is_reference]
    if (length(non_ref_dummies) > 0) {
      excluded_cols <- c(excluded_cols, non_ref_dummies)
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Excluding {length(non_ref_dummies)} dummy variable{?s} from MICE predictors: {.val {non_ref_dummies}}"
        )
      }
    }
  }

  # 2. Detect near-zero variance columns (can't predict, cause singularity)
  numeric_cols <- colnames(df)[vapply(df, is.numeric, logical(1))]
  near_zero_var <- vapply(numeric_cols, function(col) {
    vals <- df[[col]][!is.na(df[[col]])]
    if (length(vals) < 2) {
      return(TRUE)
    }
    stats::var(vals) < 1e-10
  }, logical(1))
  zero_var_cols <- numeric_cols[near_zero_var]
  if (length(zero_var_cols) > 0) {
    excluded_cols <- c(excluded_cols, zero_var_cols)
    if (get_verbosity() >= 3) {
      cli::cli_alert_info(
        "Excluding {length(zero_var_cols)} near-zero variance column{?s} from MICE: {.val {zero_var_cols}}"
      )
    }
  }

  # Remove excluded columns from MICE predictor set
  excluded_cols <- unique(excluded_cols)
  keep_cols <- setdiff(colnames(df), excluded_cols)
  df_for_mice <- df[, keep_cols, drop = FALSE]

  # Update cols_with_na to exclude already-handled columns
  cols_to_impute <- setdiff(cols_with_na, excluded_cols)
  cols_to_impute <- intersect(cols_to_impute, keep_cols)

  if (length(cols_to_impute) == 0) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("All missing values handled via pre-processing (no MICE needed)")
    }
    return(df)
  }

  # --- Run MICE with tryCatch for graceful fallback ---

  run_mice <- function() {
    mice::mice(df_for_mice, m = 1, method = "pmm", ridge = 1e-4, printFlag = get_verbosity() >= 4)
  }

  mice_obj <- tryCatch(
    {
      if (get_verbosity() >= 4) run_mice() else suppressWarnings(suppressMessages(run_mice()))
    },
    error = function(e) {
      if (grepl("singular", e$message, ignore.case = TRUE)) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning(
            "MICE imputation failed due to collinearity. Falling back to median imputation."
          )
        }
        return(NULL)
      }
      cli::cli_abort("MICE imputation failed: {conditionMessage(e)}")
    }
  )

  # Fallback to median if MICE failed
  if (is.null(mice_obj)) {
    return(handle_na_median(df, exclude_cols = exclude_cols))
  }

  # Extract completed dataset and update original
  df_imputed <- mice::complete(mice_obj, 1)
  imputed_cols <- intersect(cols_to_impute, colnames(df_imputed))
  df[, imputed_cols] <- df_imputed[, imputed_cols]

  # Check for columns MICE skipped (still have NAs) - fall back to median
  still_na_cols <- imputed_cols[vapply(imputed_cols, function(col) any(is.na(df[[col]])), logical(1))]
  if (length(still_na_cols) > 0) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info(
        "MICE skipped {length(still_na_cols)} column{?s} (likely due to collinearity), using median: {.val {still_na_cols}}"
      )
    }
    for (col in still_na_cols) {
      col_median <- stats::median(df[[col]], na.rm = TRUE)
      if (!is.na(col_median)) {
        df[[col]][is.na(df[[col]])] <- col_median
      }
    }
  }

  # --- Post-imputation: reconstruct dummy complements ---

  if (nrow(dummy_groups) > 0) {
    for (gid in unique(dummy_groups$group_id)) {
      group <- dummy_groups[dummy_groups$group_id == gid, ]
      ref_var <- group$var_name[group$is_reference]
      non_ref_vars <- group$var_name[!group$is_reference]

      # For binary dummy groups (2 vars), non-ref = 1 - ref
      if (length(group$var_name) == 2 && length(non_ref_vars) == 1 && length(ref_var) == 1) {
        if (ref_var %in% colnames(df) && non_ref_vars %in% colnames(df)) {
          # Only fill NAs in non-ref where ref was imputed
          na_mask <- is.na(df[[non_ref_vars]])
          df[[non_ref_vars]][na_mask] <- 1 - df[[ref_var]][na_mask]
        }
      }
    }
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("MICE imputation completed successfully")
  }

  df
}


#' @title Identify columns too missing to impute
#' @description Find optional numeric columns whose share of missing values
#'   exceeds `threshold`. Imputing such columns collapses them to a
#'   near-constant (e.g. a column with one valid value median-imputes into a
#'   constant), so they are better left unimputed. Required numeric columns are
#'   exempt: leaving them missing would break the analysis downstream.
#' @param df *\[data.frame\]* The data frame to analyze
#' @param threshold *\[numeric\]* Maximum tolerated missingness ratio in \[0, 1\]
#' @return *\[numeric\]* Named vector of missingness ratios, one entry per
#'   column exceeding the threshold
#' @keywords internal
identify_unimputable_columns <- function(df, threshold) {
  optional_numeric <- setdiff(
    colnames(df)[vapply(df, is.numeric, logical(1))],
    get_required_colnames()
  )
  ratios <- vapply(optional_numeric, function(col) mean(is.na(df[[col]])), numeric(1))
  ratios[ratios > threshold]
}


#' @title Handle missing values
#' @description Main function to handle missing values according to the selected strategy.
#'
#' This function handles missing values differently for required vs optional columns:
#' - Non-numeric required columns (e.g., study_id) must be complete and will cause an error if missing
#' - Numeric required columns (e.g., effect, se, n_obs) can be imputed if a non-"stop" strategy is selected
#' - Optional columns are handled according to the selected strategy, except that
#'   the "stop" strategy never aborts over them: since required columns are
#'   already guaranteed complete by the time "stop" is reached, missing values
#'   in optional columns are left as-is and reported, not treated as fatal
#'   (issue #401). A column no method actually needs should not be able to
#'   halt the whole run.
#'
#' Imputation strategies skip optional numeric columns whose missingness ratio
#' exceeds `artma.data.max_imputation_missingness`; those columns keep their
#' missing values and a warning is emitted.
#'
#' Columns the compute phase recalculates from winsorized data anyway
#' (`get_winsorization_recomputed_cols()`, issue #522) are left out entirely:
#' they are neither reported, imputed, nor allowed to trigger row removal.
#'
#' @param df *\[data.frame\]* The data frame to process
#' @return *\[data.frame\]* The processed data frame
#' @keywords internal
handle_missing_values <- function(df) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[assert],
    artma / options / typed_accessors[get_na_handling, get_max_imputation_missingness]
  )

  # Columns about to be rebuilt downstream carry values that are discarded
  # either way, so their missing values are not worth reporting or filling.
  recomputed_cols <- get_winsorization_recomputed_cols(df)
  recomputed_cols <- recomputed_cols[vapply(recomputed_cols, function(col) anyNA(df[[col]]), logical(1))]
  if (length(recomputed_cols) > 0 && get_verbosity() >= 3) {
    recomputed_counts <- vapply(recomputed_cols, function(col) sum(is.na(df[[col]])), integer(1))
    recomputed_msg <- format_cols_with_counts(recomputed_counts)
    cli::cli_alert_info(
      "Leaving missing values in {recomputed_msg} alone: recalculated from winsorized data downstream."
    )
  }

  # Detect missing values
  na_summary <- detect_missing_values(df, ignore_cols = recomputed_cols)

  # Get the handling strategy
  na_handling <- get_na_handling()

  non_numeric_required_with_na <- character(0)
  numeric_required_with_na <- character(0)

  # Check if required columns have missing values
  if (na_summary$has_required_na) {
    required_cols_with_na <- names(na_summary$required_cols_with_na)

    # Separate required columns into numeric and non-numeric
    non_numeric_required_with_na <- required_cols_with_na[
      !vapply(required_cols_with_na, function(col) is.numeric(df[[col]]), logical(1))
    ]
    numeric_required_with_na <- setdiff(required_cols_with_na, non_numeric_required_with_na)

    # Non-numeric required columns (like study_id) cannot be imputed; only the
    # "remove" strategy can resolve them, by dropping the affected rows.
    if (length(non_numeric_required_with_na) > 0) {
      non_numeric_msg <- format_cols_with_counts(
        label_with_source_names(na_summary$required_cols_with_na[non_numeric_required_with_na])
      )
      if (identical(na_handling, "remove")) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning(
            "Missing values found in non-numeric required columns: {non_numeric_msg}. The {.val remove} strategy will drop the affected rows."
          )
        }
      } else {
        cli::cli_abort(c(
          "x" = "Missing values found in non-numeric required columns: {non_numeric_msg}",
          "i" = "Non-numeric required columns (e.g., study_id) cannot be imputed and must be complete.",
          "i" = "Set {.field artma.data.na_handling} to {.val remove} to drop these rows, or clean your data before analysis."
        ))
      }
    }

    # For numeric required columns, check if strategy allows imputation
    if (length(numeric_required_with_na) > 0) {
      numeric_msg <- format_cols_with_counts(
        label_with_source_names(na_summary$required_cols_with_na[numeric_required_with_na])
      )
      if (na_handling == "stop") {
        cli::cli_abort(c(
          "x" = "Missing values found in required columns: {numeric_msg}",
          "i" = "Current strategy is {.val stop}. Change {.field artma.data.na_handling} to handle missing values automatically.",
          "i" = "Available strategies: {.val remove}, {.val median}, {.val mean}, {.val interpolate}, {.val mice}"
        ))
      }
      # If strategy is not "stop", allow processing to continue (will be handled by imputation functions)
      if (get_verbosity() >= 3) {
        cli::cli_alert_warning("Missing values detected in numeric required columns: {numeric_msg}. Will apply {.val {na_handling}} strategy.")
      }
    }
  }

  # Check if we need to process missing values
  # We need to process if:
  # 1. There are optional missing values, OR
  # 2. There are numeric required missing values and strategy is not "stop", OR
  # 3. There are non-numeric required missing values (only reachable with "remove")
  has_numeric_required_na <- length(numeric_required_with_na) > 0 && na_handling != "stop"
  has_non_numeric_required_na <- length(non_numeric_required_with_na) > 0

  needs_processing <- na_summary$has_optional_na || has_numeric_required_na || has_non_numeric_required_na

  if (!needs_processing) {
    if (get_verbosity() >= 4) {
      cli::cli_alert_success("No missing values detected that require processing")
    }
    return(df)
  }

  # Report missing values in optional columns
  if (get_verbosity() >= 3 && na_summary$has_optional_na) {
    optional_cols_msg <- format_cols_with_counts(na_summary$optional_cols_with_na)
    cli::cli_alert_warning("Missing values detected in optional columns: {optional_cols_msg}")
  }

  # Missingness-ratio guard: mostly-missing optional columns are left as-is
  # instead of being imputed into near-constants.
  skip_cols <- character(0)
  if (na_handling %in% c("median", "mean", "interpolate", "mice")) {
    max_missingness <- get_max_imputation_missingness()
    assert(
      is.numeric(max_missingness) && length(max_missingness) == 1 &&
        !is.na(max_missingness) && max_missingness >= 0 && max_missingness <= 1,
      "The option 'artma.data.max_imputation_missingness' must be a single number between 0 and 1."
    )
    skip_ratios <- identify_unimputable_columns(df, max_missingness)
    skip_ratios <- skip_ratios[setdiff(names(skip_ratios), recomputed_cols)]
    skip_cols <- names(skip_ratios)
    if (length(skip_cols) > 0 && get_verbosity() >= 2) {
      skip_msg <- paste0(
        skip_cols, " (", round(skip_ratios * 100, 1), "% missing)",
        collapse = ", "
      )
      cli::cli_alert_warning(
        "Skipping imputation for {length(skip_cols)} column{?s} with more than {.val {max_missingness * 100}%} missing values: {skip_msg}."
      )
      cli::cli_alert_info(
        "These columns keep their missing values; adjust {.field artma.data.max_imputation_missingness} to change this."
      )
    }
  }

  exclude_cols <- union(skip_cols, recomputed_cols)

  # Apply the selected strategy
  df_processed <- switch(na_handling,
    # By this point required columns are guaranteed complete: a numeric or
    # non-numeric required NA would already have aborted above. So "stop"
    # only has optional-column NAs left to deal with, and those are not
    # worth failing the whole run over (issue #401) - they are left as-is.
    "stop" = {
      optional_cols_msg <- format_cols_with_counts(label_with_source_names(na_summary$optional_cols_with_na))
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Leaving missing values in optional columns as-is (not used by every method): {optional_cols_msg}"
        )
        cli::cli_alert_info(
          "The {.val stop} strategy only requires required columns to be complete. Set {.field artma.data.na_handling} to {.val remove}, {.val median}, {.val mean}, {.val interpolate}, or {.val mice} to handle these values automatically."
        )
      }
      df
    },
    "remove" = handle_na_remove(df, exclude_cols = recomputed_cols),
    "median" = handle_na_median(df, exclude_cols = exclude_cols),
    "mean" = handle_na_mean(df, exclude_cols = exclude_cols),
    "interpolate" = handle_na_interpolate(df, exclude_cols = exclude_cols),
    "mice" = handle_na_mice(df, exclude_cols = exclude_cols),
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
