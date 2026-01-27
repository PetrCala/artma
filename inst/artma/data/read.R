#' @title Detect column types from text data
#' @description Analyzes entire columns to determine appropriate R types.
#' Uses the full column data, not just a sample, for accurate type detection.
#' @param df *\[data.frame\]* Data frame with all columns as character/text
#' @return *\[character\]* Vector of R types ("logical", "numeric", "character") for each column
#' @keywords internal
detect_excel_column_types <- function(df) {
  box::use(artma / const[CONST])

  n_cols <- ncol(df)
  types <- character(n_cols)

  for (i in seq_len(n_cols)) {
    col_data <- df[[i]]
    col_name <- colnames(df)[i]

    # Remove NA values for analysis
    non_na <- col_data[!is.na(col_data)]

    # Empty column or all NA
    if (length(non_na) == 0) {
      types[i] <- "character"
      next
    }

    # Try to detect logical values
    # Check if all non-NA values are logical-like (TRUE/FALSE, T/F, 1/0, yes/no)
    logical_patterns <- c("TRUE", "FALSE", "T", "F", "1", "0", "yes", "no", "YES", "NO", "Yes", "No")
    if (all(non_na %in% logical_patterns) && length(non_na) > 0) {
      # If all values match logical patterns, treat as logical
      # This handles columns that are consistently logical throughout
      types[i] <- "logical"
      next
    }

    # Try to coerce to numeric
    numeric_coerced <- suppressWarnings(as.numeric(non_na))
    na_after_coercion <- sum(is.na(numeric_coerced))
    na_before_coercion <- sum(is.na(non_na))

    # If most values can be coerced to numeric, treat as numeric
    # Allow some non-numeric values (up to 10% or if original had NAs)
    conversion_rate <- (length(non_na) - na_after_coercion) / length(non_na)
    if (conversion_rate >= 0.9 || (na_after_coercion == na_before_coercion && length(non_na) > 0)) {
      types[i] <- "numeric"
    } else {
      # Default to character for mixed or non-numeric content
      types[i] <- "character"
    }
  }

  types
}

#' @title Coerce columns to detected types
#' @description Safely coerces columns to their detected types, handling NA strings and edge cases.
#' @param df *\[data.frame\]* Data frame with all columns as character/text
#' @param types *\[character\]* Vector of R types to coerce to
#' @return *\[data.frame\]* Data frame with columns coerced to appropriate types
#' @keywords internal
coerce_columns_to_types <- function(df, types) {
  box::use(
    artma / const[CONST],
    artma / libs / core / utils[get_verbosity]
  )

  for (i in seq_len(ncol(df))) {
    col_name <- colnames(df)[i]
    target_type <- types[i]
    col_data <- df[[i]]

    if (target_type == "logical") {
      # Coerce to logical, handling various representations
      # First, normalize NA strings
      na_mask <- col_data %in% CONST$DATA$NA_STRINGS | is.na(col_data)

      # Initialize result vector
      logical_result <- rep(NA, length(col_data))

      # Convert logical-like strings
      true_patterns <- c("TRUE", "T", "1", "yes", "YES", "Yes")
      false_patterns <- c("FALSE", "F", "0", "no", "NO", "No")

      logical_result[col_data %in% true_patterns] <- TRUE
      logical_result[col_data %in% false_patterns] <- FALSE
      logical_result[na_mask] <- NA

      df[[i]] <- logical_result
    } else if (target_type == "numeric") {
      # Coerce to numeric, handling NA strings
      na_mask <- col_data %in% CONST$DATA$NA_STRINGS | is.na(col_data)
      col_data[na_mask] <- NA_character_

      # Convert to numeric, non-coercible values become NA with warning
      numeric_col <- suppressWarnings(as.numeric(col_data))
      failed_coercions <- sum(is.na(numeric_col) & !na_mask)

      if (failed_coercions > 0 && get_verbosity() >= 3) {
        cli::cli_alert_warning(
          "Column {.val {col_name}}: {failed_coercions} value{?s} could not be coerced to numeric and were set to NA"
        )
      }

      df[[i]] <- numeric_col
    } else {
      # Keep as character, but normalize NA strings
      na_mask <- col_data %in% CONST$DATA$NA_STRINGS
      col_data[na_mask] <- NA_character_
      df[[i]] <- col_data
    }
  }

  df
}

#' @title Read Excel file with consistent NA handling and smart type detection
#' @description Read Excel files (xlsx, xls, xlsm) with standardized NA string handling.
#' Uses a two-pass approach: first reads all columns as text, then analyzes entire columns
#' to determine appropriate types, eliminating type guessing warnings and improving accuracy.
#' @param path *\[character\]* Path to the Excel file
#' @return *\[data.frame\]* The data frame with correctly typed columns
#' @keywords internal
read_excel_file <- function(path) {
  box::use(
    artma / const[CONST],
    artma / libs / core / utils[get_verbosity]
  )

  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg readxl} is required to read Excel files. Install with: install.packages('readxl')")
  }

  if (get_verbosity() >= 4) {
    cli::cli_inform("Reading Excel file with smart type detection...")
  }

  # First pass: Read all columns as text to avoid type guessing issues
  # This ensures we capture all data without warnings, then we'll intelligently determine types
  df_text <- suppressWarnings(
    readxl::read_excel(
      path,
      col_types = "text", # Read everything as text
      na = CONST$DATA$NA_STRINGS,
      trim_ws = TRUE
    )
  )

  # Second pass: Detect types from entire columns (not just first 1000 rows)
  if (get_verbosity() >= 4) {
    cli::cli_inform("Analyzing column types from full dataset...")
  }
  column_types <- detect_excel_column_types(df_text)

  # Third pass: Coerce columns to detected types
  if (get_verbosity() >= 4) {
    cli::cli_inform("Coercing columns to detected types...")
  }
  df_typed <- coerce_columns_to_types(df_text, column_types)

  df_typed
}

#' @title Read data
#' @description Read data from a path. Returns a data frame with smart handling of various formats.
#' @param path *\[str, optional\]* The path to the data source. If NULL, the options data source path is used.
#' @return *\[data.frame\]* The data frame.
read_data <- function(path = NULL) {
  box::use(
    artma / data / utils[
      determine_df_type,
      raise_invalid_data_type_error,
      standardize_column_names
    ],
    artma / data / smart_detection[
      smart_read_csv,
      validate_df_structure
    ],
    artma / libs / core / utils[get_verbosity]
  )

  path <- if (!is.null(path)) path else getOption("artma.data.source_path")

  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("No data source path provided. Please set {.field artma.data.source_path} option.")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Data file not found: {.path {path}}")
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Reading data from {.path {path}}")
  }

  df_type <- determine_df_type(path, should_validate = TRUE)

  # Read based on file type with enhanced handling
  df <- tryCatch(
    {
      switch(df_type,
        csv = smart_read_csv(path),
        tsv = smart_read_csv(path, delim = "\t"),
        xlsx = read_excel_file(path),
        xls = read_excel_file(path),
        xlsm = read_excel_file(path),
        json = {
          if (!requireNamespace("jsonlite", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg jsonlite} is required to read JSON files. Install with: install.packages('jsonlite')")
          }
          jsonlite::fromJSON(path)
        },
        dta = {
          if (!requireNamespace("haven", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg haven} is required to read Stata files. Install with: install.packages('haven')")
          }
          as.data.frame(haven::read_dta(path))
        },
        rds = {
          obj <- readRDS(path)
          if (!is.data.frame(obj)) {
            cli::cli_abort("RDS file does not contain a data frame. Found: {.type {class(obj)}}")
          }
          obj
        },
        raise_invalid_data_type_error(df_type)
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to read data from {.path {path}}",
        "i" = "File type: {.val {df_type}}",
        "i" = "Error: {e$message}"
      ))
    }
  )

  if (!is.data.frame(df)) {
    cli::cli_abort("Failed to read data from {.path {path}}. Expected a data frame but got {.type {class(df)}}.")
  }

  # Validate and clean structure
  df <- validate_df_structure(df, path)

  # Standardize column names based on user options
  df <- standardize_column_names(df)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Data read successfully: {nrow(df)} rows, {ncol(df)} columns")
  }

  df
}

box::export(read_data)
