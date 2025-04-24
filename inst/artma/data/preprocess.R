#' @title Standardize column names
#' @description DERPRECATED Standardize the column names of a data frame to a single source of truth set of column names.
#' @param df *\[data.frame\]* The data frame to standardize
#' @param map *\[list\]* A list of column name mappings
#' @return *\[data.frame\]* The standardized data frame
standardize_column_names <- function(df, map = NULL) {
  box::use(
    artma / data / utils[get_required_colnames],
    artma / options / utils[get_option_group],
    artma / libs / validation[assert, validate]
  )

  validate(is.data.frame(df))

  map <- if (is.null(map)) get_option_group("artma.data.colnames") else map
  map <- lapply(map, make.names) # Handle non-standard column names

  required_colnames <- get_required_colnames()

  # Check that every required column is mapped in the user options file
  missing_required <- base::setdiff(required_colnames, names(map))
  if (length(missing_required)) {
    cli::cli_abort("Missing mapping for required columns: {.val {missing_required}}")
  }

  # Check that every required column exists in the data frame
  mapped_cols <- unlist(unname(map[names(map) %in% required_colnames]))
  absent_required <- mapped_cols[!mapped_cols %in% names(df)]
  if (length(absent_required)) {
    cli::cli_abort("These required columns are absent in the data frame: {.val {absent_required}}")
  }

  # Rename columns to standardized names - only when present in the data frame
  reverse_map <- stats::setNames(names(map), unlist(map))
  names(df)[names(df) %in% names(reverse_map)] <- reverse_map[names(df)[names(df) %in% names(reverse_map)]]

  missing_required <- setdiff(required_colnames, colnames(df))
  if (length(missing_required)) {
    cli::cli_abort("Failed to standardize the following columns: {.val {missing_required}}")
  }

  df
}

#' @title Preprocess the raw data
#' @param input_data *\[data.frame\]* Main data frame
#' @return *\[data.frame\]* The preprocessed data
preprocess_data <- function(input_data) { # nolint: cyclocomp_linter
  box::use(
    artma / data_config / read[get_data_config],
    artma / data_config / utils[get_config_values],
    artma / libs / validation[validate]
  )

  validate(is.data.frame(input_data))

  config <- get_data_config()

  # Compute optional columns
  # TODO

  # Remove redundant columns
  expected_col_n <- length(config)
  cli::cli_inform("Removing redundant columns...")
  while (ncol(input_data) > expected_col_n) {
    col_to_remove <- colnames(input_data)[ncol(input_data)]
    if (!all(is.na(input_data[[col_to_remove]]))) {
      cli::cli_abort("Cannot remove column {.val {col_to_remove}} as it contains non-NA values.")
    }
    input_data <- input_data[, -ncol(input_data)]
  }

  # Variable name validity check
  varnames <- colnames(input_data)
  expected_varnames <- get_config_values(config, "var_name")

  # Check if all columns of the first vector are in the second one and vice versa
  cli::cli_inform("Checking variable names...")
  if (!all(varnames %in% expected_varnames) || !all(expected_varnames %in% varnames)) {
    missing_from_var_list <- varnames[!varnames %in% expected_varnames]
    missing_from_data <- expected_varnames[!expected_varnames %in% varnames]
    cli::cli_abort(c(
      "x" = "Mismatching variable names.",
      "i" = "These variables are not a part of the variable list: {.val {missing_from_var_list}}",
      "i" = "These variables are not a part of the main data frame columns: {.val {missing_from_data}}"
    ))
  }

  # Check for correct ordering
  cli::cli_inform("Checking variable names order...")
  if (!identical(varnames, expected_varnames)) {
    problematic_indexes <- which(varnames != expected_varnames)
    cli::cli_abort(c(
      "x" = "The order of some columns in the data frame and the expected variable list is different.",
      "i" = "Problematic indexes and their column names: {.val {problematic_indexes}}",
      "i" = "Data frame has '{.val {varnames[problematic_indexes]}}' but expected variable list has '{.val {expected_varnames[problematic_indexes]}}'."
    ))
  }

  # Remove redundant rows
  cli::cli_inform("Removing redundant rows...")
  study_col <- getOption("artma.data.colnames.study")
  while (is.na(input_data[nrow(input_data), study_col])) {
    input_data <- input_data[-nrow(input_data), ]
  }

  # Preprocess and enforce correct data types
  cli::cli_inform("Enforcing correct data types...")
  for (col_name in make.names(varnames)) {
    col_data_type <- config[[col_name]]$data_type
    if (col_data_type == "int" || col_data_type == "dummy") {
      input_data[[col_name]] <- as.integer(input_data[[col_name]])
    } else if (col_data_type == "float" || col_data_type == "perc") {
      input_data[[col_name]] <- as.numeric(input_data[[col_name]])
    } else if (col_data_type == "category") {
      input_data[[col_name]] <- as.character(input_data[[col_name]])
    }
  }
  cli::cli_alert_success("Preprocessing finished.")
  return(input_data)
}

box::export(
  preprocess_data,
  standardize_column_names
)
