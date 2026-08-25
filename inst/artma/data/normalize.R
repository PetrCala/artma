#' @title Repair a character vector that is not valid UTF-8
#' @description Datasets exported from Excel, Stata, or older statistical
#'   software are routinely single-byte encoded (Windows-1252 or Latin-1) while
#'   carrying no encoding declaration, so a name like \code{"Müller"} reaches R
#'   as bytes that are not valid UTF-8. Base R's regex engine treats such
#'   strings as a hard error rather than a warning whenever a pattern needs
#'   character boundaries (any character class or anchor), which turns an
#'   ordinary accented author name into a crash deep inside column detection.
#'   Values that are already valid UTF-8 are never touched, so this only ever
#'   repairs text that would otherwise be unusable.
#' @param x *\[character\]* The vector to repair.
#' @return *\[character\]* The vector with every element valid UTF-8.
#' @keywords internal
repair_utf8 <- function(x) {
  if (!is.character(x) || length(x) == 0L) {
    return(x)
  }

  bad <- which(!is.na(x) & !validUTF8(x))
  if (length(bad) == 0L) {
    return(x)
  }

  # Windows-1252 first: it is a superset of Latin-1 across the printable range
  # and the encoding behind the overwhelming majority of undeclared exports.
  # Latin-1 then catches the five byte values Windows-1252 leaves undefined.
  for (from in c("windows-1252", "latin1")) {
    converted <- iconv(x[bad], from = from, to = "UTF-8")
    ok <- !is.na(converted) & validUTF8(converted)
    if (any(ok)) {
      x[bad[ok]] <- converted[ok]
      bad <- bad[!ok]
    }
    if (length(bad) == 0L) {
      return(x)
    }
  }

  # Anything still unreadable is mixed or corrupt. Substitute the offending
  # bytes rather than dropping the value: a study label with one mangled
  # character still identifies its study, an NA does not. Whatever iconv cannot
  # salvage it returns as NA, which keeps the guarantee that every element of
  # the result is valid UTF-8.
  x[bad] <- iconv(x[bad], from = "UTF-8", to = "UTF-8", sub = "?")
  x
}


#' @title Ensure every column name and text value is valid UTF-8
#' @description Applies \code{repair_utf8} to the column names, character
#'   columns, and factor levels of a freshly read data frame. This is the single
#'   place encoding is normalized: everything downstream (whitespace
#'   normalization, column recognition, schema reconciliation) may then assume
#'   it is matching against valid UTF-8.
#' @param df *\[data.frame\]* The freshly read data frame.
#' @return *\[data.frame\]* The data frame with all text valid UTF-8.
#' @keywords internal
ensure_utf8_columns <- function(df) {
  box::use(artma / libs / core / log[log_info])

  repaired_names <- repair_utf8(names(df))
  n_repaired_names <- sum(repaired_names != names(df), na.rm = TRUE)
  names(df) <- repaired_names

  repaired_cols <- character(0)
  for (col in seq_along(df)) {
    x <- df[[col]]
    if (is.character(x)) {
      fixed <- repair_utf8(x)
      if (!identical(fixed, x)) {
        repaired_cols <- c(repaired_cols, names(df)[[col]])
        df[[col]] <- fixed
      }
    } else if (is.factor(x)) {
      fixed_levels <- repair_utf8(levels(x))
      if (!identical(fixed_levels, levels(x))) {
        repaired_cols <- c(repaired_cols, names(df)[[col]])
        levels(x) <- fixed_levels
        df[[col]] <- x
      }
    }
  }

  if (n_repaired_names > 0L || length(repaired_cols) > 0L) {
    log_info(paste(
      "The data source is not UTF-8 encoded. Repaired text in",
      "{length(repaired_cols)} column{?s}{cli::qty(length(repaired_cols))}",
      if (length(repaired_cols) > 0L) "({.field {repaired_cols}})" else "",
      "so that accented characters read correctly.",
      "Re-export the file as UTF-8 to silence this."
    ))
  }

  df
}


#' @title Replace NA-strings with NA
#' @description Replace values listed in \code{CONST$DATA$NA_STRINGS} with a real
#'   \code{NA} across every character (and character-coercible factor) column.
#'   Numeric and logical columns produced by native readers are left untouched.
#' @param df *\[data.frame\]* The data frame to normalize
#' @return *\[data.frame\]* The data frame with NA-strings replaced by NA
#' @keywords internal
replace_na_strings <- function(df) {
  box::use(artma / const[CONST])

  na_strings <- CONST$DATA$NA_STRINGS
  for (col in colnames(df)) {
    x <- df[[col]]
    if (is.character(x)) {
      x[x %in% na_strings] <- NA_character_
      df[[col]] <- x
    } else if (is.factor(x)) {
      if (any(levels(x) %in% na_strings)) {
        x <- as.character(x)
        x[x %in% na_strings] <- NA_character_
        df[[col]] <- x
      }
    }
  }
  df
}


#' @title Coerce character columns to their natural R type
#' @description Convert text columns (as read from a file) to logical, integer,
#'   or numeric when every non-NA value is consistent with that type. This uses
#'   \code{utils::type.convert}, the same inference base R applies to CSV
#'   columns, so a value like \code{"1.5"} becomes numeric and \code{"TRUE"}
#'   becomes logical regardless of the source format. The resulting types feed
#'   the \code{determine_vector_type} classification used downstream when
#'   building the data config. NA-string and whitespace normalization must run
#'   first so blanks do not block coercion. Columns that are not uniformly
#'   coercible stay character.
#' @param df *\[data.frame\]* The data frame whose character columns to coerce
#' @return *\[data.frame\]* The data frame with columns coerced to natural types
#' @keywords internal
coerce_df_columns <- function(df) {
  for (col in colnames(df)) {
    x <- df[[col]]
    if (is.character(x) && any(!is.na(x))) {
      df[[col]] <- utils::type.convert(x, as.is = TRUE)
    }
  }
  df
}


#' @title Normalize a freshly read data frame
#' @description Shared post-read normalization applied to every input format:
#'   repair non-UTF-8 text, replace NA-strings with NA, convert whitespace-only
#'   strings to NA, and coerce character columns to their natural R type.
#'   Running this for all formats guarantees that, for example, \code{"NA"}
#'   becomes \code{NA} whether the file was CSV, Excel, JSON, Stata, or RDS.
#'   Encoding repair runs first because every later step matches regexes against
#'   these values and base R errors on invalid UTF-8.
#' @param df *\[data.frame\]* The freshly read data frame
#' @return *\[data.frame\]* The normalized data frame
#' @keywords internal
normalize_read_df <- function(df) {
  box::use(artma / data / smart_detection[normalize_whitespace_to_na])

  df |>
    ensure_utf8_columns() |>
    replace_na_strings() |>
    normalize_whitespace_to_na() |>
    coerce_df_columns()
}


box::export(
  repair_utf8,
  ensure_utf8_columns,
  replace_na_strings,
  coerce_df_columns,
  normalize_read_df
)
