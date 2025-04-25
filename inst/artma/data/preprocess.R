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
  cli::cli_inform("Removing redundant columns…")
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
  varnames <- colnames(df)
  config <- get_data_config()
  expected_varnames <- get_config_values(config, "var_name")

  cli::cli_inform("Checking variable names…")
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
  cli::cli_inform("Checking variable name order…")
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

#' @title Check that required columns are non-empty
#' @description Check that required columns are non-empty.
#' @param df *\[data.frame\]* The data frame to check that required columns are non-empty for
#' @return *\[data.frame\]* The data frame with the redundant columns removed
#' @keywords internal
check_required_non_empty <- function(df) {
  cli::cli_inform("Checking that required columns are non-empty…")
  required_colnames <- get_required_colnames()
  col_lengths <- vapply(df[, required_colnames, drop = FALSE], function(x) sum(!is.na(x)), numeric(1))
  if (length(unique(col_lengths)) > 1) {
    cli::cli_abort(c(
      "x" = "Required columns have different counts of non-NA values.",
      "i" = "{required_colnames}: {col_lengths}",
      "i" = "Make sure none have empty cells."
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
  cli::cli_inform("Removing empty rows…")
  required_colnames <- get_required_colnames()
  na_rows <- rev(which(is.na(df[, required_colnames, drop = FALSE])))
  if (length(na_rows)) {
    df <- df[-na_rows, ]
    cli::cli_alert_success("Removed {.val {length(na_rows)}} empty rows.")
  }
  df
}

#' @title Enforce correct data types
#' @description Enforce correct data types.
#' @param df *\[data.frame\]* The data frame to enforce correct data types for
#' @return *\[data.frame\]* The data frame with the correct data types enforced
#' @keywords internal
enforce_data_types <- function(df) {
  cli::cli_inform("Enforcing correct data types…")
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
    check_required_non_empty() %>%
    remove_empty_rows() %>%
    enforce_data_types()
}

box::export(preprocess_data)
