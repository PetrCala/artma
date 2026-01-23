#' @title Detect delimiter in CSV-like file
#' @description Intelligently detect the delimiter used in a CSV file
#' @param path *\[character\]* Path to the file
#' @param n_lines *\[integer\]* Number of lines to sample for detection
#' @return *\[character\]* The detected delimiter
detect_delimiter <- function(path, n_lines = 5) {
  box::use(artma / libs / validation[validate])

  validate(is.character(path), file.exists(path))

  # Read first few lines
  lines <- readLines(path, n = n_lines, warn = FALSE)

  if (length(lines) == 0) {
    return(",")
  }

  # Common delimiters to test
  delimiters <- c(",", ";", "\t", "|")

  # Count occurrences of each delimiter in each line
  counts <- vapply(delimiters, function(delim) {
    mean(vapply(lines, function(line) {
      length(gregexpr(delim, line, fixed = TRUE)[[1]])
    }, integer(1)))
  }, numeric(1))

  # Also check consistency (should appear same number of times per line)
  consistency <- vapply(delimiters, function(delim) {
    line_counts <- vapply(lines, function(line) {
      length(gregexpr(delim, line, fixed = TRUE)[[1]])
    }, integer(1))
    if (max(line_counts) == 0) {
      return(0)
    }
    1 - (stats::sd(line_counts) / max(max(line_counts), 1))
  }, numeric(1))

  # Score = count * consistency
  scores <- counts * consistency

  if (all(scores == 0)) {
    return(",") # Default fallback
  }

  delimiters[which.max(scores)]
}


#' @title Detect file encoding
#' @description Attempt to detect the file encoding
#' @param path *\[character\]* Path to the file
#' @return *\[character\]* The detected encoding
detect_encoding <- function(path) {
  box::use(artma / libs / validation[validate])

  validate(is.character(path), file.exists(path))

  # Try reading with different encodings
  encodings <- c("UTF-8", "latin1", "ISO-8859-1", "CP1252")

  for (enc in encodings) {
    test <- tryCatch(
      {
        readLines(path, n = 10, encoding = enc, warn = FALSE)
        enc
      },
      error = function(e) NULL
    )
    if (!is.null(test)) {
      return(enc)
    }
  }

  "UTF-8" # Fallback
}


#' @title Smart read CSV with auto-detection
#' @description Read CSV file with automatic delimiter and encoding detection
#' @param path *\[character\]* Path to the file
#' @param delim *\[character, optional\]* Delimiter (auto-detected if NULL)
#' @param encoding *\[character, optional\]* Encoding (auto-detected if NULL)
#' @return *\[data.frame\]* The data frame
smart_read_csv <- function(path, delim = NULL, encoding = NULL) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[validate],
    artma / libs / utils[get_verbosity]
  )

  validate(is.character(path), file.exists(path))

  if (is.null(delim)) {
    delim <- detect_delimiter(path)
    if (get_verbosity() >= 4) {
      cli::cli_inform("Auto-detected delimiter: {.val {delim}}")
    }
  }

  if (is.null(encoding)) {
    encoding <- detect_encoding(path)
    if (get_verbosity() >= 4) {
      cli::cli_inform("Auto-detected encoding: {.val {encoding}}")
    }
  }

  # Try reading with detected parameters
  df <- tryCatch(
    {
      utils::read.table(
        path,
        header = TRUE,
        sep = delim,
        fileEncoding = encoding,
        stringsAsFactors = FALSE,
        na.strings = CONST$DATA$NA_STRINGS,
        strip.white = TRUE,
        comment.char = "",
        quote = "\""
      )
    },
    error = function(e) {
      # Fallback: try with default read.csv
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Failed to read with detected parameters, trying fallback method")
      }
      tryCatch(
        utils::read.csv(path, stringsAsFactors = FALSE, na.strings = CONST$DATA$NA_STRINGS),
        error = function(e2) {
          cli::cli_abort(c(
            "x" = "Failed to read CSV file: {.path {path}}",
            "i" = "Original error: {e$message}",
            "i" = "Fallback error: {e2$message}"
          ))
        }
      )
    }
  )

  df
}


#' @title Normalize whitespace-only strings to NA
#' @description Convert columns where values are whitespace-only strings to NA.
#' This ensures that rows with only whitespace are properly detected as empty.
#' Handles both character columns and columns that might be read as character but should be numeric.
#' @param df *\[data.frame\]* The data frame to normalize
#' @return *\[data.frame\]* The data frame with whitespace-only strings converted to NA
#' @keywords internal
normalize_whitespace_to_na <- function(df) {
  for (col in colnames(df)) {
    # Handle character columns (most common case)
    if (is.character(df[[col]])) {
      # Convert whitespace-only strings to NA
      # Matches: empty string, or strings containing only whitespace characters (space, tab, newline, etc.)
      whitespace_only <- grepl("^\\s*$", df[[col]])
      df[[col]][whitespace_only] <- NA_character_
    } else if (is.factor(df[[col]])) {
      # Also handle factor columns (which might contain whitespace)
      # Convert factor levels that are whitespace-only to NA
      levels_whitespace <- grepl("^\\s*$", levels(df[[col]]))
      if (any(levels_whitespace)) {
        # Convert whitespace factor levels to NA
        df[[col]] <- as.character(df[[col]])
        whitespace_only <- grepl("^\\s*$", df[[col]])
        df[[col]][whitespace_only] <- NA_character_
      }
    }
  }
  df
}


#' @title Validate and clean data frame structure
#' @description Check for and fix common data frame issues. Normalizes whitespace-only strings to NA
#' before detecting empty rows and columns, ensuring consistent handling across all data formats.
#' @param df *\[data.frame\]* The data frame to validate
#' @param path *\[character\]* Original file path (for error messages)
#' @return *\[data.frame\]* Cleaned data frame
validate_df_structure <- function(df, path) {
  box::use(
    artma / libs / validation[validate],
    artma / libs / utils[get_verbosity]
  )

  validate(is.data.frame(df))

  # Check if data frame is empty
  if (nrow(df) == 0) {
    cli::cli_abort("The data frame read from {.path {path}} is empty (0 rows).")
  }

  if (ncol(df) == 0) {
    cli::cli_abort("The data frame read from {.path {path}} has no columns.")
  }

  # Normalize whitespace-only strings to NA (must happen before empty row/column detection)
  df <- normalize_whitespace_to_na(df)

  # Check for completely empty columns (parsing artifacts)
  empty_cols <- vapply(df, function(col) all(is.na(col)), logical(1))
  if (any(empty_cols)) {
    empty_col_names <- names(df)[empty_cols]
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Removing {length(empty_col_names)} empty column{?s}: {.val {empty_col_names}}")
    }
    df <- df[, !empty_cols, drop = FALSE]
  }

  # Check for duplicate column names
  if (any(duplicated(names(df)))) {
    dup_names <- unique(names(df)[duplicated(names(df))])
    cli::cli_alert_warning(c(
      "!" = "Found duplicate column names: {.val {dup_names}}",
      "i" = "Making names unique..."
    ))
    names(df) <- make.unique(names(df), sep = "_")
  }

  # Remove trailing empty rows (common in Excel exports)
  all_na_rows <- apply(df, 1, function(row) all(is.na(row)))
  if (any(all_na_rows)) {
    n_removed <- sum(all_na_rows)
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Removing {n_removed} trailing empty row{?s}")
    }
    df <- df[!all_na_rows, , drop = FALSE]
  }

  # Check if we still have data after cleaning
  if (nrow(df) == 0) {
    cli::cli_abort("After cleaning, the data frame from {.path {path}} is empty.")
  }

  df
}


box::export(
  detect_delimiter,
  detect_encoding,
  smart_read_csv,
  validate_df_structure,
  normalize_whitespace_to_na
)
