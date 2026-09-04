#' @title Detect delimiter in CSV-like file
#' @description Intelligently detect the delimiter used in a CSV file
#' @param path *\[character\]* Path to the file
#' @param n_lines *\[integer\]* Number of lines to sample for detection
#' @return *\[character\]* The detected delimiter
detect_delimiter <- function(path, n_lines = 5) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(path), file.exists(path))

  # Read first few lines. These are raw bytes straight off disk, before the
  # post-read encoding repair in `normalize_read_df`, so a single-byte encoded
  # file reaches the counting below as invalid UTF-8. Every candidate delimiter
  # is ASCII and no ASCII byte can occur inside a multi-byte sequence in any
  # encoding we read, so counting bytes is both correct here and immune to the
  # locale warnings character-wise matching would raise.
  lines <- readLines(path, n = n_lines, warn = FALSE)

  if (length(lines) == 0) {
    return(",")
  }

  # Common delimiters to test
  delimiters <- c(",", ";", "\t", "|")

  # Count occurrences of each delimiter in each line
  counts <- vapply(delimiters, function(delim) {
    mean(vapply(lines, function(line) {
      length(gregexpr(delim, line, fixed = TRUE, useBytes = TRUE)[[1]])
    }, integer(1)))
  }, numeric(1))

  # Also check consistency (should appear same number of times per line)
  consistency <- vapply(delimiters, function(delim) {
    line_counts <- vapply(lines, function(line) {
      length(gregexpr(delim, line, fixed = TRUE, useBytes = TRUE)[[1]])
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


UTF8_BOM <- as.raw(c(0xEF, 0xBB, 0xBF))


#' @title Detect a UTF-8 byte order mark
#' @description Reports whether a file starts with the UTF-8 BOM (`EF BB BF`).
#'   Excel's "CSV UTF-8" export and Windows Notepad write one; left in place,
#'   `utils::read.table()` folds the three bytes into the first header name and
#'   `make.names()` renders them as `X...<name>`, so the first column loses its
#'   identity silently.
#' @param path *\[character\]* Path to the file
#' @return *\[logical\]* TRUE when the file starts with a UTF-8 BOM
has_utf8_bom <- function(path) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(path), file.exists(path))

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  head_bytes <- readBin(con, what = "raw", n = 3L)
  length(head_bytes) == 3L && identical(head_bytes, UTF8_BOM)
}


#' @title Drop a leading UTF-8 BOM from a string
#' @description Byte-level so that it behaves the same in every locale and
#'   never touches the rest of the line, which may still be raw single-byte
#'   encoded text awaiting repair in `normalize_read_df`.
#' @param x *\[character(1)\]* The first line of a file
#' @return *\[character(1)\]* The line without its BOM
#' @keywords internal
strip_utf8_bom <- function(x) {
  bytes <- charToRaw(x)
  if (length(bytes) < 3L || !identical(bytes[1:3], UTF8_BOM)) {
    return(x)
  }
  rawToChar(bytes[-(1:3)])
}


#' @title Open a text connection positioned past the UTF-8 BOM
#' @description Reads the header line, strips the BOM, and pushes the line
#'   back onto the connection so the reader sees the file as if the BOM had
#'   never been written. This avoids `fileEncoding = "UTF-8-BOM"`, which
#'   re-encodes the whole file into the native encoding and truncates
#'   non-ASCII values in a C locale. The caller closes the connection.
#' @param path *\[character\]* Path to the file
#' @return *\[connection\]* An open text-mode connection
#' @keywords internal
open_past_bom <- function(path) {
  con <- file(path, open = "rt")
  # Hand the connection to the caller only once it is positioned; close it
  # ourselves if the header read fails.
  positioned <- FALSE
  on.exit(if (!positioned) close(con), add = TRUE)
  first_line <- readLines(con, n = 1L, warn = FALSE)
  if (length(first_line) == 1L) {
    pushBack(strip_utf8_bom(first_line), con)
  }
  positioned <- TRUE
  con
}


#' @title Detect the decimal separator in a delimited file
#' @description Decide whether numbers in a delimited text file use a decimal
#'   comma (\code{"0,817"}) or a decimal point. The header line is skipped and
#'   the fields of the next \code{n_lines} lines are matched against the
#'   comma-decimal shape (\code{CONST$DATA$DECIMAL_COMMA_PATTERN}) and its
#'   dot-decimal counterpart. A comma wins when at least one field has the
#'   comma shape and comma-shaped fields outnumber dot-shaped ones, so a file
#'   with thousands separators next to dot decimals still reads with a point.
#'   A comma-delimited file can never use comma decimals and always returns
#'   \code{"."}.
#' @param path *\[character\]* Path to the file
#' @param delim *\[character\]* The field delimiter the file uses
#' @param n_lines *\[integer\]* Number of data lines to sample after the header
#' @return *\[character\]* \code{","} or \code{"."}
detect_decimal_separator <- function(path, delim, n_lines = 20) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate]
  )

  validate(is.character(path), file.exists(path), is.character(delim))

  if (delim == ",") {
    return(".")
  }

  # Same raw-bytes reasoning as in detect_delimiter: the file has not been
  # through encoding repair yet, and every character we match is ASCII.
  lines <- readLines(path, n = n_lines + 1, warn = FALSE)
  if (length(lines) < 2) {
    return(".")
  }

  fields <- unlist(strsplit(lines[-1], delim, fixed = TRUE, useBytes = TRUE))
  fields <- gsub("[ \t\r\"]", "", fields, useBytes = TRUE)
  dot_pattern <- sub(",", "\\.", CONST$DATA$DECIMAL_COMMA_PATTERN, fixed = TRUE)

  n_comma <- sum(grepl(CONST$DATA$DECIMAL_COMMA_PATTERN, fields, useBytes = TRUE))
  n_dot <- sum(grepl(dot_pattern, fields, useBytes = TRUE))

  if (n_comma > 0 && n_comma > n_dot) "," else "."
}


#' @title Smart read CSV with auto-detection
#' @description Read CSV file with automatic delimiter and decimal separator
#'   detection. A file written under a European locale (\code{;} between
#'   fields, \code{,} inside numbers) reads its numeric columns as numeric
#'   rather than as text.
#' @param path *\[character\]* Path to the file
#' @param delim *\[character, optional\]* Delimiter (auto-detected if NULL)
#' @param dec *\[character, optional\]* Decimal separator (auto-detected if NULL)
#' @return *\[data.frame\]* The data frame
smart_read_csv <- function(path, delim = NULL, dec = NULL) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / log[log_info]
  )

  validate(is.character(path), file.exists(path))

  if (is.null(delim)) {
    delim <- detect_delimiter(path)
    if (get_verbosity() >= 4) {
      cli::cli_inform("Auto-detected delimiter: {.val {delim}}")
    }
  }

  if (is.null(dec)) {
    dec <- detect_decimal_separator(path, delim)
    if (dec == ",") {
      log_info("Detected a comma decimal separator in {.path {path}}; reading numbers with {.code dec = \",\"}")
    }
  }

  # A leading BOM would otherwise be read as part of the first header name.
  # BOM-prefixed files are read through a connection positioned past it; every
  # other file keeps the plain path so nothing changes for them.
  has_bom <- has_utf8_bom(path)
  read_input <- function(reader) {
    if (!has_bom) {
      return(reader(path))
    }
    con <- open_past_bom(path)
    on.exit(close(con), add = TRUE)
    reader(con)
  }

  # Try reading with detected parameters
  df <- tryCatch(
    {
      read_input(function(input) {
        utils::read.table(
          input,
          header = TRUE,
          sep = delim,
          dec = dec,
          stringsAsFactors = FALSE,
          na.strings = CONST$DATA$NA_STRINGS,
          strip.white = TRUE,
          comment.char = "",
          quote = "\""
        )
      })
    },
    error = function(e) {
      # Fallback: try with default read.csv
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Failed to read with detected parameters, trying fallback method")
      }
      tryCatch(
        read_input(function(input) {
          utils::read.csv(input, dec = dec, stringsAsFactors = FALSE, na.strings = CONST$DATA$NA_STRINGS)
        }),
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
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity]
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
  all_na_rows <- rowSums(is.na(df)) == ncol(df)
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
  has_utf8_bom,
  detect_decimal_separator,
  smart_read_csv,
  validate_df_structure,
  normalize_whitespace_to_na
)
