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
#'   replace NA-strings with NA, convert whitespace-only strings to NA, and
#'   coerce character columns to their natural R type. Running this for all
#'   formats guarantees that, for example, \code{"NA"} becomes \code{NA} whether
#'   the file was CSV, Excel, JSON, Stata, or RDS.
#' @param df *\[data.frame\]* The freshly read data frame
#' @return *\[data.frame\]* The normalized data frame
#' @keywords internal
normalize_read_df <- function(df) {
  box::use(artma / data / smart_detection[normalize_whitespace_to_na])

  df |>
    replace_na_strings() |>
    normalize_whitespace_to_na() |>
    coerce_df_columns()
}


box::export(
  replace_na_strings,
  coerce_df_columns,
  normalize_read_df
)
