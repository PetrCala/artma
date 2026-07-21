box::use(
  artma / data / utils[get_required_colnames],
  artma / data_config / read[get_data_config],
  artma / data / smart_detection[normalize_whitespace_to_na]
)


#' @title Remove empty rows
#' @description Remove rows that are empty or have missing critical required columns.
#' A row is considered empty if all required columns are NA, or if the critical
#' required columns (study_id, effect, se) are all missing, regardless of n_obs.
#' @param df *\[data.frame\]* The data frame to remove empty rows from
#' @return *\[data.frame\]* The data frame with the empty rows removed
#' @keywords internal
remove_empty_rows <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Removing empty rows...")
  }

  required_colnames <- get_required_colnames()
  required_colnames <- required_colnames[required_colnames %in% colnames(df)]

  # Critical required columns that must be present for a row to be valid.
  # n_obs is deliberately excluded: it can be missing/imputed downstream,
  # while the remaining required columns (study_id, effect, se) are essential.
  critical_cols <- setdiff(required_colnames, "n_obs")
  critical_cols <- critical_cols[critical_cols %in% colnames(df)]

  # A row is empty when all required columns are NA, or when all critical
  # columns are missing (even if n_obs has a value). Both checks are vectorized
  # with rowSums(is.na(...)) instead of iterating rows.
  all_required_na <- if (length(required_colnames) > 0) {
    rowSums(is.na(df[, required_colnames, drop = FALSE])) == length(required_colnames)
  } else {
    rep(FALSE, nrow(df))
  }

  all_critical_na <- if (length(critical_cols) > 0) {
    rowSums(is.na(df[, critical_cols, drop = FALSE])) == length(critical_cols)
  } else {
    rep(FALSE, nrow(df))
  }

  empty_rows <- all_required_na | all_critical_na

  if (any(empty_rows)) {
    df <- df[!empty_rows, , drop = FALSE]
    cli::cli_alert_success("Removed {.val {sum(empty_rows)}} empty rows.")
  }
  df
}

#' @title Resolve the missing-value handling strategy
#' @description Configure-phase decision that fixes `artma.data.na_handling`
#'   before the cached compute phase runs. When the option is already set it is
#'   a no-op. Otherwise, if the (already cleaned) data frame has missing values
#'   in optional columns, it prompts the user interactively (and offers to save
#'   the choice) or falls back to the deterministic `"stop"` strategy in a
#'   non-interactive session. This is intentionally separate from
#'   `handle_missing_values()` (the pure compute-phase step) so that no prompt
#'   or option write ever happens inside the cached pipeline.
#' @param df *\[data.frame\]* The cleaned data frame used to detect optional
#'   missing values (as produced by `clean_data()`).
#' @return `NULL`, invisibly.
#' @keywords internal
resolve_na_handling <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / na_handling[detect_missing_values],
    artma / options / prompts[prompt_na_handling],
    artma / interactive / save_preference[prompt_save_preference]
  )

  na_handling_option <- getOption("artma.data.na_handling", default = NA)

  # Strategy already configured: nothing to decide.
  if (!is.na(na_handling_option)) {
    return(invisible(NULL))
  }

  na_summary <- detect_missing_values(df)

  # Without optional missing values there is no decision to make; the compute
  # phase falls back to the deterministic "stop" default when it runs.
  if (!na_summary$has_optional_na) {
    return(invisible(NULL))
  }

  if (interactive()) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Missing values detected and no handling strategy configured")
    }

    # Prompt the user for their preference and record it for this session.
    selected_strategy <- prompt_na_handling()
    options(artma.data.na_handling = selected_strategy)

    # Offer to persist the preference to the options file.
    prompt_save_preference(
      option_path = "data.na_handling",
      value = selected_strategy,
      description = "missing value handling strategy",
      respect_autonomy = FALSE
    )
  } else {
    # Non-interactive mode: default to "stop"
    if (get_verbosity() >= 2) {
      cli::cli_warn("Running in non-interactive mode with missing values. Defaulting to 'stop' strategy.")
    }
    options(artma.data.na_handling = "stop")
  }

  invisible(NULL)
}


#' @title Enforce correct data types
#' @description Enforce correct data types. Every column of the data frame
#'   must have a corresponding data config entry; a missing entry means the
#'   config is out of sync with the data and raises an error.
#' @param df *\[data.frame\]* The data frame to enforce correct data types for
#' @return *\[data.frame\]* The data frame with the correct data types enforced
#' @keywords internal
enforce_data_types <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Enforcing correct data types...")
  }

  config <- get_data_config()
  col_names <- colnames(df)
  col_keys <- make.names(col_names)

  missing_cols <- col_names[!col_keys %in% names(config)]
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "No data config entry found for column{?s} {.val {missing_cols}}.",
      "i" = "The data config is out of sync with the data frame.",
      "i" = "Run {.code artma::config.fix()} to regenerate the config from the data."
    ))
  }

  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    dtype <- config[[col_keys[i]]]$data_type
    if (is.null(dtype) || length(dtype) != 1 || is.na(dtype)) {
      # Entries merged from sparse overrides may carry no type information;
      # there is nothing to coerce to in that case.
      next
    }
    if (dtype %in% c("int", "dummy")) {
      df[[col_name]] <- as.integer(df[[col_name]])
    } else if (dtype %in% c("float", "perc")) {
      df[[col_name]] <- as.numeric(df[[col_name]])
    } else if (dtype == "category") {
      df[[col_name]] <- as.character(df[[col_name]])
    }
  }
  df
}

#' @title Check for invalid values
#' @description Check for invalid values and enforce correct ones
#' @param df *\[data.frame\]* The data frame to check for invalid values for
#' @return *\[data.frame\]* The data frame with the invalid values enforced
#' @keywords internal
enforce_correct_values <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking for invalid values...")
  }

  box::use(artma / libs / core / validation[assert])

  se_zero_handling <- getOption("artma.calc.se_zero_handling", "stop")

  zero_se_rows <- which(df$se == 0)

  if (se_zero_handling == "stop") {
    assert(length(zero_se_rows) == 0, "The 'se' column contains zero values")
  } else if (se_zero_handling == "warn") {
    if (length(zero_se_rows) > 0) {
      if (get_verbosity() >= 3) {
        cli::cli_warn("The 'se' column contains zero values in {length(zero_se_rows)} rows")
      }
    }
  }

  df
}

#' @title Winsorize data
#' @description Winsorize effect and standard error columns at specified quantiles.
#' @param df *\[data.frame\]* The data frame to winsorize
#' @return *\[data.frame\]* The data frame with winsorized values
#' @keywords internal
winsorize_data <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  winsorization_level <- getOption("artma.data.winsorization_level", default = 0)

  # Skip if winsorization is disabled
  if (is.null(winsorization_level) || is.na(winsorization_level) || winsorization_level == 0) {
    if (get_verbosity() >= 4) {
      cli::cli_inform("Winsorization disabled (level = 0)")
    }
    return(df)
  }

  if (get_verbosity() >= 3) {
    cli::cli_inform("Winsorizing data at {.val {winsorization_level}} level...")
  }

  # Winsorize effect column
  if ("effect" %in% colnames(df)) {
    lower_q <- stats::quantile(df$effect, probs = winsorization_level, na.rm = TRUE)
    upper_q <- stats::quantile(df$effect, probs = 1 - winsorization_level, na.rm = TRUE)

    n_lower <- sum(df$effect < lower_q, na.rm = TRUE)
    n_upper <- sum(df$effect > upper_q, na.rm = TRUE)

    df$effect <- pmax(pmin(df$effect, upper_q), lower_q)

    if (get_verbosity() >= 3 && (n_lower + n_upper) > 0) {
      cli::cli_alert_info("Winsorized {.val {n_lower + n_upper}} effect values ({n_lower} lower, {n_upper} upper)")
    }
  }

  # Winsorize standard error column
  if ("se" %in% colnames(df)) {
    lower_q <- stats::quantile(df$se, probs = winsorization_level, na.rm = TRUE)
    upper_q <- stats::quantile(df$se, probs = 1 - winsorization_level, na.rm = TRUE)

    n_lower <- sum(df$se < lower_q, na.rm = TRUE)
    n_upper <- sum(df$se > upper_q, na.rm = TRUE)

    df$se <- pmax(pmin(df$se, upper_q), lower_q)

    if (get_verbosity() >= 3 && (n_lower + n_upper) > 0) {
      cli::cli_alert_info("Winsorized {.val {n_lower + n_upper}} SE values ({n_lower} lower, {n_upper} upper)")
    }
  }

  df
}


#' @title Clean data
#' @description Normalize blank cells to `NA` and drop empty rows. This is the
#'   pre-missing-value cleaning shared by the configure phase (which detects
#'   optional missing values on the cleaned frame to decide the NA strategy) and
#'   the compute phase (`preprocess_data`). It performs no prompts and no option
#'   writes, so it is safe to run in either phase.
#' @param df *[data.frame]* Standardized data frame to clean.
#' @return *[data.frame]* The cleaned data frame.
#' @keywords internal
clean_data <- function(df) {
  df |>
    normalize_whitespace_to_na() |>
    remove_empty_rows()
}

#' @title Preprocess data
#' @description Preprocess a standardized data frame: removes empty rows, handles
#'   missing values, enforces data types, winsorizes, and validates values.
#'   Column presence validation is handled upstream by the schema reconciliation
#'   step; this function assumes the dataframe columns already match the config.
#'   It is pure: the missing-value strategy is resolved beforehand by
#'   `resolve_na_handling()` in the configure phase, so this function never
#'   prompts or writes options and can run inside the cached compute phase.
#' @param df *[data.frame]* Standardized data frame to clean.
#' @return *[data.frame]* The type-safe, cleaned data frame.
preprocess_data <- function(df) {
  box::use(artma / data / na_handling[handle_missing_values])

  df |>
    clean_data() |>
    handle_missing_values() |>
    enforce_data_types() |>
    winsorize_data() |>
    enforce_correct_values()
}

box::export(clean_data, enforce_data_types, preprocess_data, resolve_na_handling)
