box::use(
  artma / data / utils[get_required_colnames],
  artma / data_config / read[get_data_config],
  artma / data_config / utils[get_config_values]
)

#' @title Remove redundant columns
#' @description Remove columns that are not expected in the data frame.
#' @param df *\[data.frame\]* The data frame to remove columns from
#' @return *\[data.frame\]* The data frame with the redundant columns removed
#' @keywords internal
remove_redundant_columns <- function(df) {
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Removing redundant columns…")
  }
  expected_col_n <- length(get_data_config())
  while (ncol(df) > expected_col_n) {
    col_to_remove <- colnames(df)[ncol(df)]
    if (!all(is.na(df[[col_to_remove]]))) {
      cli::cli_abort("Cannot remove column {.val {col_to_remove}} as it contains non-NA values.")
    }
    df <- df[, -ncol(df)]
  }
  df
}

#' @title Verify variable names
#' @description Verify that the variable names in the data frame match the expected variable names.
#' @param df *\[data.frame\]* The data frame to verify variable names for
#' @return *\[data.frame\]* The data frame with the redundant columns removed
#' @keywords internal
verify_variable_names <- function(df) {
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking variable names…")
  }

  varnames <- colnames(df)
  config <- get_data_config()
  expected_varnames <- get_config_values(config, "var_name")

  if (!setequal(varnames, expected_varnames)) {
    missing_from_var_list <- setdiff(varnames, expected_varnames)
    missing_from_data <- setdiff(expected_varnames, varnames)
    cli::cli_abort(c(
      "x" = "Mismatching variable names.",
      "i" = "Not in variable list: {.val {missing_from_var_list}}",
      "i" = "Not in data frame: {.val {missing_from_data}}"
    ))
  }
  df
}


#' @title Verify variable order
#' @description Verify that the variable order in the data frame matches the expected variable order.
#' @param df *\[data.frame\]* The data frame to verify variable order for
#' @return *\[data.frame\]* The data frame with the redundant columns removed
#' @keywords internal
verify_variable_order <- function(df) {
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking variable name order…")
  }

  varnames <- colnames(df)
  config <- get_data_config()
  expected_varnames <- get_config_values(config, "var_name")
  if (!identical(varnames, expected_varnames)) {
    problematic <- which(varnames != expected_varnames)
    cli::cli_abort(c(
      "x" = "Column order differs from expected list.",
      "i" = "Problematic indexes: {.val {problematic}}",
      "i" = "DF: '{.val {varnames[problematic]}}'; Expected: '{.val {expected_varnames[problematic]}}'"
    ))
  }
  df
}


#' @title Remove empty rows
#' @description Remove rows that are empty.
#' @param df *\[data.frame\]* The data frame to remove empty rows from
#' @return *\[data.frame\]* The data frame with the empty rows removed
#' @keywords internal
remove_empty_rows <- function(df) {
  box::use(
    artma / libs / utils[get_verbosity],
    artma / libs / polyfills[map_lgl]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Removing empty rows…")
  }

  required_colnames <- get_required_colnames()
  na_rows <- which(map_lgl(seq_len(nrow(df)), ~ all(is.na(df[., required_colnames, drop = FALSE]))))
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
    artma / libs / utils[get_verbosity],
    artma / data / na_handling[detect_missing_values, handle_missing_values],
    artma / options / prompts[prompt_na_handling]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking for missing values…")
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

      # Ask if they want to save this preference
      save_preference <- climenu::select(
        choices = c("Yes, save to options file", "No, use only for this session"),
        prompt = "Do you want to save this preference to your options file?"
      )

      if (!rlang::is_empty(save_preference) && save_preference == 1) {
        # Save to options file
        tryCatch(
          {
            box::use(artma / options / files[update_option_in_file])
            options_file <- getOption("artma.temp.file_name")
            if (!is.null(options_file)) {
              update_option_in_file(options_file, "data.na_handling", selected_strategy)
              if (get_verbosity() >= 3) {
                cli::cli_alert_success("Preference saved to options file")
              }
            }
          },
          error = function(e) {
            cli::cli_alert_warning("Could not save preference to options file: {e$message}")
          }
        )
      }
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
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Enforcing correct data types…")
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
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Checking for invalid values…")
  }

  box::use(artma / libs / validation[assert])

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
  box::use(artma / libs / utils[get_verbosity])

  winsorization_level <- getOption("artma.data.winsorization_level", default = 0)

  # Skip if winsorization is disabled
  if (is.null(winsorization_level) || is.na(winsorization_level) || winsorization_level == 0) {
    if (get_verbosity() >= 4) {
      cli::cli_inform("Winsorization disabled (level = 0)")
    }
    return(df)
  }

  if (get_verbosity() >= 3) {
    cli::cli_inform("Winsorizing data at {.val {winsorization_level}} level…")
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
#' @description Preprocess the raw data frame.
#' @param df *[data.frame]* Raw data frame to clean.
#' @return *[data.frame]* The validated, type‑safe, and trimmed data frame.
preprocess_data <- function(df) {
  box::use(magrittr[`%>%`])

  df %>%
    remove_redundant_columns() %>%
    verify_variable_names() %>%
    verify_variable_order() %>%
    remove_empty_rows() %>%
    handle_missing_values_with_prompt() %>%
    enforce_data_types() %>%
    winsorize_data() %>%
    enforce_correct_values()
}

box::export(preprocess_data)
