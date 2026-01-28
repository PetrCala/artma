box::use(
  artma / data / utils[get_required_colnames],
  artma / data_config / read[get_data_config],
  artma / data_config / utils[get_config_values],
  artma / data / smart_detection[normalize_whitespace_to_na]
)

#' @title Handle extra columns with data
#' @description Handle columns that contain data but aren't in the expected config.
#' @param df *\[data.frame\]* The data frame
#' @param data_cols *\[character\]* Vector of column names that contain data
#' @return *\[data.frame\]* The data frame (potentially modified)
#' @keywords internal
handle_extra_columns_with_data <- function(df, data_cols) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data_config / write[update_data_config],
    artma / const[CONST],
    artma / data / utils[determine_vector_type],
    artma / libs / core / string[make_verbose_name]
  )

  strategy <- getOption("artma.data.extra_columns_strategy", default = "keep")

  if (strategy == "abort") {
    cli::cli_abort(c(
      "x" = "Found {length(data_cols)} extra column{?s} with data that are not in the data config: {.val {data_cols}}",
      "i" = "Set {.code options(artma.data.extra_columns_strategy = \"keep\")} to automatically include them",
      "i" = "Or set {.code options(artma.data.extra_columns_strategy = \"remove\")} to remove them (data loss)"
    ))
  }

  if (strategy == "remove") {
    if (get_verbosity() >= 2) {
      cli::cli_warn(c(
        "!" = "Removing {length(data_cols)} extra column{?s} with data: {.val {data_cols}}",
        "i" = "This will result in data loss. Consider using {.code options(artma.data.extra_columns_strategy = \"keep\")}"
      ))
    }
    df <- df[, !colnames(df) %in% data_cols, drop = FALSE]
    return(df)
  }

  if (strategy == "prompt") {
    if (!interactive()) {
      # Non-interactive mode: fall back to "keep" strategy
      if (get_verbosity() >= 2) {
        cli::cli_warn("Running in non-interactive mode. Falling back to 'keep' strategy for extra columns.")
      }
      strategy <- "keep"
    } else {
      for (col in data_cols) {
        choice <- climenu::menu(
          choices = c(
            "Keep column (add to data config)",
            "Remove column (data loss)",
            "Abort"
          ),
          prompt = paste0("Column '", col, "' contains data but is not in the data config. What would you like to do?")
        )

        if (is.null(choice)) {
          cli::cli_abort("Column mapping cancelled by user")
        }

        if (choice == "Keep column (add to data config)") {
          # Keep - will be handled below
        } else if (choice == "Remove column (data loss)") {
          df <- df[, colnames(df) != col, drop = FALSE]
          data_cols <- setdiff(data_cols, col)
          if (get_verbosity() >= 3) {
            cli::cli_alert_info("Removed column {.val {col}}")
          }
        } else {
          cli::cli_abort("Aborted by user")
        }
      }
    }
  }

  # Strategy is "keep" or user chose to keep in prompt mode
  if (length(data_cols) > 0) {
    # Add these columns to the data config
    config_changes <- list()

    for (col in data_cols) {
      col_name_clean <- make.names(col)
      col_data <- df[[col]]

      col_data_type <- tryCatch(
        determine_vector_type(
          data = col_data,
          recognized_data_types = CONST$DATA_CONFIG$DATA_TYPES
        ),
        error = function(e) {
          if (get_verbosity() >= 2) {
            cli::cli_alert_warning("Failed to determine data type for column {.val {col}}, using 'unknown'")
          }
          "unknown"
        }
      )

      config_changes[[col_name_clean]] <- list(
        "var_name" = col,
        "var_name_verbose" = make_verbose_name(col),
        "var_name_description" = make_verbose_name(col),
        "data_type" = col_data_type,
        "group_category" = NA,
        "na_handling" = getOption("artma.data.na_handling"),
        "variable_summary" = is.numeric(col_data),
        "effect_sum_stats" = NA,
        "equal" = NA,
        "gltl" = NA,
        "bma" = NA,
        "bma_reference_var" = NA,
        "bma_to_log" = NA,
        "bpe" = NA,
        "bpe_sum_stats" = NA,
        "bpe_equal" = NA,
        "bpe_gltl" = NA
      )
    }

    if (length(config_changes) > 0) {
      suppressMessages(update_data_config(changes = config_changes))
      if (get_verbosity() >= 3) {
        cli::cli_alert_success("Added {length(config_changes)} extra column{?s} to data config: {.val {data_cols}}")
      }
    }
  }

  df
}

#' @title Remove redundant columns
#' @description Remove columns that are not expected in the data frame.
#' Uses name-based identification rather than position-based removal.
#' @param df *\[data.frame\]* The data frame to remove columns from
#' @return *\[data.frame\]* The data frame with the redundant columns removed
#' @keywords internal
remove_redundant_columns <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Removing redundant columns...")
  }

  # Get expected column names from config
  config <- get_data_config()
  expected_varnames <- get_config_values(config, "var_name")
  expected_varnames_clean <- make.names(expected_varnames)

  # Identify redundant columns (in df but not expected)
  # Apply make.names to df column names for consistent comparison
  df_colnames <- colnames(df)
  df_colnames_clean <- make.names(df_colnames)
  redundant_cols <- setdiff(df_colnames_clean, expected_varnames_clean)

  # Map back to original column names for removal
  redundant_cols_original <- df_colnames[df_colnames_clean %in% redundant_cols]

  if (length(redundant_cols_original) == 0) {
    return(df) # No redundant columns
  }

  # Separate into empty vs non-empty
  empty_cols <- character(0)
  data_cols <- character(0)

  for (col in redundant_cols_original) {
    if (all(is.na(df[[col]]))) {
      empty_cols <- c(empty_cols, col)
    } else {
      data_cols <- c(data_cols, col)
    }
  }

  # Remove empty columns silently
  if (length(empty_cols) > 0) {
    df <- df[, !colnames(df) %in% empty_cols, drop = FALSE]
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("Removed {length(empty_cols)} empty redundant column{?s}: {.val {empty_cols}}")
    }
  }

  # Handle columns with data
  if (length(data_cols) > 0) {
    df <- handle_extra_columns_with_data(df, data_cols)
  }

  df
}

#' @title Verify variable names
#' @description Verify that all expected non-computed columns exist in the data frame and no extra columns are present.
#' Computed columns (marked with is_computed = TRUE in config) are allowed to be missing as they will be computed later.
#' Column order does not matter - only the presence of all expected non-computed columns and absence of unexpected ones.
#' @param df *\[data.frame\]* The data frame to verify variable names for
#' @return *\[data.frame\]* The data frame (unchanged, validation only)
#' @keywords internal
verify_variable_names <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking variable names...")
  }

  # Get expected column names from config
  config <- get_data_config()
  expected_varnames <- get_config_values(config, "var_name")

  # Filter out NA values (edge case: config might have invalid entries)
  expected_varnames <- expected_varnames[!is.na(expected_varnames)]

  # Identify computed columns (columns that will be computed later, not required in raw data)
  computed_varnames <- character(0)
  for (config_key in names(config)) {
    config_entry <- config[[config_key]]
    if (isTRUE(config_entry$is_computed)) {
      var_name <- config_entry$var_name
      if (!is.na(var_name)) {
        computed_varnames <- c(computed_varnames, var_name)
      }
    }
  }

  # Only require non-computed columns to exist in the data
  required_varnames <- setdiff(expected_varnames, computed_varnames)

  # Get actual column names
  varnames <- colnames(df)

  # Check for missing required (non-computed) columns
  missing_from_data <- setdiff(required_varnames, varnames)
  
  # Check for unexpected extra columns (excluding computed columns that might be in config but not in data yet)
  extra_in_data <- setdiff(varnames, expected_varnames)

  if (length(missing_from_data) > 0 || length(extra_in_data) > 0) {
    cli::cli_abort(c(
      "x" = "Column name mismatch.",
      "i" = "All expected non-computed columns must exist, and no extra columns are allowed.",
      "i" = "Missing required columns: {.val {missing_from_data}}",
      "i" = "Unexpected columns: {.val {extra_in_data}}"
    ))
  }

  df
}


#' @title Remove empty rows
#' @description Remove rows that are empty or have missing critical required columns.
#' A row is considered empty if all required columns are NA, or if the critical
#' required columns (study, effect, se) are all missing, regardless of n_obs.
#' @param df *\[data.frame\]* The data frame to remove empty rows from
#' @return *\[data.frame\]* The data frame with the empty rows removed
#' @keywords internal
remove_empty_rows <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / infrastructure / polyfills[map_lgl]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Removing empty rows...")
  }

  required_colnames <- get_required_colnames()

  # Critical required columns that must be present for a row to be valid
  # n_obs can be missing/imputed, but study, effect, and se are essential
  critical_cols <- c("study", "effect", "se")
  critical_cols <- critical_cols[critical_cols %in% colnames(df)]

  # Remove rows where all required columns are NA
  all_na_rows <- which(map_lgl(seq_len(nrow(df)), ~ all(is.na(df[., required_colnames, drop = FALSE]))))

  # Also remove rows where all critical columns are missing (even if n_obs has a value)
  # This catches rows that have n_obs but are missing the essential analysis columns
  critical_na_rows <- if (length(critical_cols) > 0) {
    which(map_lgl(seq_len(nrow(df)), ~ all(is.na(df[., critical_cols, drop = FALSE]))))
  } else {
    integer(0)
  }

  # Combine both sets of rows to remove
  na_rows <- unique(c(all_na_rows, critical_na_rows))

  if (length(na_rows)) {
    df <- df[-na_rows, ]
    cli::cli_alert_success("Removed {.val {length(na_rows)}} empty rows.")
  }
  df
}

#' @title Check that required columns contain no empty cells and handle optional missing values
#' @description Check required columns for missing values (always errors) and handle optional column missing values based on strategy.
#' @param df *\[data.frame\]* The data frame to check and process
#' @return *\[data.frame\]* The data frame with missing values handled
#' @keywords internal
handle_missing_values_with_prompt <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / na_handling[detect_missing_values, handle_missing_values],
    artma / options / prompts[prompt_na_handling],
    artma / interactive / save_preference[prompt_save_preference]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking for missing values...")
  }

  # Detect all missing values
  na_summary <- detect_missing_values(df)

  # Check if we need to prompt the user
  na_handling_option <- getOption("artma.data.na_handling", default = NA)

  # If option is not set AND there are optional missing values, prompt the user
  if (is.na(na_handling_option) && na_summary$has_optional_na) {
    if (interactive()) {
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("Missing values detected and no handling strategy configured")
      }

      # Prompt the user for their preference
      selected_strategy <- prompt_na_handling()

      # Set the option for the current session
      options(artma.data.na_handling = selected_strategy)

      # Use the reusable save preference function
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
  }

  # Now handle missing values using the configured strategy
  df_processed <- handle_missing_values(df)

  df_processed
}



#' @title Enforce correct data types
#' @description Enforce correct data types.
#' @param df *\[data.frame\]* The data frame to enforce correct data types for
#' @return *\[data.frame\]* The data frame with the correct data types enforced
#' @keywords internal
enforce_data_types <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Enforcing correct data types...")
  }

  config <- get_data_config()
  for (col_name in make.names(colnames(df))) {
    dtype <- config[[col_name]]$data_type
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

  se_zero_handling <- getOption("artma.calc.se_zero_handling")

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



#' @title Preprocess data
#' @description Preprocess the raw data frame. Validates column names (all expected columns must exist, no extra columns allowed), removes empty rows and columns, handles missing values, enforces data types, and applies winsorization.
#' Column order does not matter - columns are identified by name only.
#' @param df *[data.frame]* Raw data frame to clean.
#' @return *[data.frame]* The validated, type-safe, and trimmed data frame.
preprocess_data <- function(df) {
  df |>
    remove_redundant_columns() |>
    verify_variable_names() |>
    normalize_whitespace_to_na() |>
    remove_empty_rows() |>
    handle_missing_values_with_prompt() |>
    enforce_data_types() |>
    normalize_whitespace_to_na() |>
    remove_empty_rows() |>
    winsorize_data() |>
    enforce_correct_values()
}

box::export(
  preprocess_data,
  remove_redundant_columns,
  verify_variable_names
)
