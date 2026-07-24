#' @title Shared column profiler
#' @description
#' Single home for the value-level column profiling primitives used across the
#' package: numeric/uniqueness/variance/sequential profiling (`profile_column`),
#' the binary-column predicate (`is_binary_column`), and the dummy-group
#' detector (`detect_dummy_groups`). Living in `data/` lets both the data
#' pipeline (`data/na_handling.R`, `data/column_recognition.R`) and the variable
#' layer (`variable/detection.R`, `variable/suggestion.R`) share one
#' implementation without the data layer importing from `variable/`.

box::use(
  artma / libs / core / validation[validate]
)

#' @title Profile column values to determine semantic properties
#' @description Analyzes actual data values to help discriminate between
#'   ambiguous column matches. Reports numeric-ness, sequential pattern,
#'   uniqueness, and basic statistics.
#' @param values *\[vector\]* Column values to analyze
#' @return *\[list\]* Analysis results with various heuristics
#' @export
profile_column <- function(values) {
  # Remove NA values for analysis
  values_clean <- values[!is.na(values)]

  if (length(values_clean) == 0) {
    return(list(
      is_sequential = FALSE,
      is_unique = FALSE,
      is_numeric = FALSE,
      uniqueness_ratio = 0,
      mean = NA,
      variance = NA,
      min = NA,
      max = NA
    ))
  }

  # Check if numeric (coercible to numeric)
  is_numeric <- is.numeric(values_clean) || !any(is.na(suppressWarnings(as.numeric(values_clean))))
  numeric_values <- if (is_numeric) {
    if (is.numeric(values_clean)) values_clean else as.numeric(values_clean)
  } else {
    numeric(0)
  }

  # Sequential pattern detection (like 1, 2, 3, 4, 5...)
  is_sequential <- FALSE
  if (is_numeric && length(numeric_values) >= 3) {
    diffs <- diff(numeric_values)
    # Check if differences are constant (allowing for some tolerance)
    is_sequential <- all(abs(diffs - diffs[1]) < 1e-10) && abs(diffs[1] - 1) < 1e-10
  }

  # Uniqueness analysis
  is_unique <- length(unique(values_clean)) == length(values_clean)
  uniqueness_ratio <- length(unique(values_clean)) / length(values_clean)

  # Statistical properties
  stats <- if (is_numeric && length(numeric_values) > 0) {
    list(
      mean = mean(numeric_values, na.rm = TRUE),
      variance = stats::var(numeric_values, na.rm = TRUE),
      min = min(numeric_values, na.rm = TRUE),
      max = max(numeric_values, na.rm = TRUE)
    )
  } else {
    list(mean = NA, variance = NA, min = NA, max = NA)
  }

  list(
    is_sequential = is_sequential,
    is_unique = is_unique,
    is_numeric = is_numeric,
    uniqueness_ratio = uniqueness_ratio,
    mean = stats$mean,
    variance = stats$variance,
    min = stats$min,
    max = stats$max
  )
}


#' @title Detect if a column is a binary (0/1 or TRUE/FALSE) column
#' @description A column is binary when it has exactly two distinct non-missing
#'   values, both drawn from `{0, 1, TRUE, FALSE}`. On numeric input this is
#'   equivalent to requiring both values to be in `{0, 1}` (the logical values
#'   coerce to the same numerics), so it preserves the semantics of both the
#'   dummy-group detector and the variable-suggestion binary branch.
#' @param values *\[vector\]* Column values to test
#' @return *\[logical\]* TRUE if the column is binary
#' @export
is_binary_column <- function(values) {
  vals <- unique(values[!is.na(values)])
  length(vals) == 2 && all(vals %in% c(0, 1, TRUE, FALSE))
}


#' Empty group-detection result skeleton
#'
#' @return Zero-row data frame with the group-detection columns
#' @keywords internal
#' @export
empty_group_df <- function() {
  data.frame(
    var_name = character(0),
    group_id = character(0),
    group_type = character(0),
    group_base = character(0),
    is_reference = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Build a single group-detection result row
#'
#' @keywords internal
#' @export
group_row <- function(var_name, group_id, group_type, group_base, is_reference) {
  data.frame(
    var_name = var_name,
    group_id = group_id,
    group_type = group_type,
    group_base = group_base,
    is_reference = is_reference,
    stringsAsFactors = FALSE
  )
}

#' Bind group-detection rows, returning the empty skeleton when there are none
#'
#' @keywords internal
#' @export
bind_group_rows <- function(results) {
  if (length(results)) do.call(rbind, results) else empty_group_df()
}


#' Detect dummy variable groups
#'
#' @description
#' Identifies groups of dummy (binary 0/1) variables that share a common prefix,
#' suggesting they represent categories of the same underlying variable.
#'
#' @param var_names *\[character\]* Vector of variable names
#' @param df *\[data.frame\]* Data frame to check if variables are binary
#'
#' @return Data frame with group information for dummy variables
#'
#' @keywords internal
#' @export
detect_dummy_groups <- function(var_names, df) {
  validate(is.character(var_names), is.data.frame(df))

  results <- list()

  # Find variables with common prefixes
  binary_vars <- var_names[vapply(var_names, function(var_name) {
    if (!var_name %in% names(df)) {
      return(FALSE)
    }
    is_binary_column(df[[var_name]])
  }, logical(1))]

  if (!length(binary_vars)) {
    return(empty_group_df())
  }

  # Extract potential base names (everything before last underscore)
  extract_base <- function(name) {
    parts <- strsplit(name, "_")[[1]]
    if (length(parts) > 1) {
      paste(parts[-length(parts)], collapse = "_")
    } else {
      NA_character_
    }
  }

  bases <- vapply(binary_vars, extract_base, character(1))
  base_table <- table(bases[!is.na(bases)])

  # Only consider groups with 2+ variables
  valid_bases <- names(base_table)[base_table >= 2]

  if (!length(valid_bases)) {
    return(empty_group_df())
  }

  for (base in valid_bases) {
    group_vars <- binary_vars[!is.na(bases) & bases == base]

    # Try to detect reference variable (often has "other", "ref", "base" suffix)
    ref_patterns <- c("other", "ref", "reference", "base", "baseline")
    is_ref <- rep(FALSE, length(group_vars))

    for (i in seq_along(group_vars)) {
      suffix <- tolower(sub(paste0("^", base, "_"), "", group_vars[i]))
      if (suffix %in% ref_patterns) {
        is_ref[i] <- TRUE
      }
    }

    # If no explicit reference, mark the first as reference
    if (!any(is_ref) && length(group_vars) > 0) {
      is_ref[1] <- TRUE
    }

    for (i in seq_along(group_vars)) {
      results[[length(results) + 1]] <- group_row(
        var_name = group_vars[i],
        group_id = paste0("dummy_", base),
        group_type = "dummy",
        group_base = base,
        is_reference = is_ref[i]
      )
    }
  }

  bind_group_rows(results)
}


box::export(
  profile_column,
  is_binary_column,
  empty_group_df,
  group_row,
  bind_group_rows,
  detect_dummy_groups
)
