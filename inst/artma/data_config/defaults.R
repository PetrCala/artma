#' @title Build Default Config Entry
#' @description Build a default config entry for a single dataframe column.
#' @param col_name *\[character\]* The original column name
#' @param col_data *\[vector\]* The column data vector
#' @return *\[list\]* A list representing the default config for this column
build_default_config_entry <- function(col_name, col_data) {
  box::use(
    artma / const[CONST],
    artma / data / utils[determine_vector_type],
    artma / libs / core / string[make_verbose_name],
    artma / libs / core / utils[get_verbosity]
  )

  col_name_verbose <- make_verbose_name(col_name)

  col_data_type <- tryCatch(
    determine_vector_type(
      data = col_data,
      recognized_data_types = CONST$DATA_CONFIG$DATA_TYPES
    ),
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Failed to determine the data type of the column {.val {col_name}}."
        )
      }
      "unknown"
    }
  )

  list(
    "var_name" = col_name,
    "var_name_verbose" = col_name_verbose,
    "var_name_description" = col_name_verbose,
    "data_type" = col_data_type,
    "group_category" = NA,
    "na_handling" = getOption("artma.data.na_handling", NA),
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

#' @title Build Base Config
#' @description Build the full default config for all columns in a dataframe.
#'   This represents the "base layer" -- all auto-derivable values.
#' @param df *\[data.frame\]* The dataframe to build config for
#' @return *\[list\]* Named list of config entries, keyed by `make.names(col)`
build_base_config <- function(df) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df))

  if (nrow(df) == 0) {
    cli::cli_abort(
      "The dataframe is empty. Please provide a dataframe with at least one row."
    )
  }

  config <- list()

  for (col in names(df)) {
    col_key <- make.names(col)
    config[[col_key]] <- build_default_config_entry(col, df[[col]])
  }

  config
}

#' @title Compare Two Values Treating NA == NA as TRUE
#' @description Helper for comparing config values where both being NA
#'   should be considered equal.
#' @param a *\[any\]* First value
#' @param b *\[any\]* Second value
#' @return *\[logical\]* TRUE if values are identical (including both-NA case)
identical_or_both_na <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)
  if (length(a) == 1 && length(b) == 1 && is.na(a) && is.na(b)) return(TRUE)
  identical(a, b)
}

#' @title Extract Overrides
#' @description Diff a config entry against its default, returning only
#'   fields that differ from the defaults.
#' @param entry *\[list\]* The full config entry
#' @param default_entry *\[list\]* The default config entry for the same column
#' @return *\[list|NULL\]* A list containing only non-default fields, or NULL
#'   if all fields match defaults
extract_overrides <- function(entry, default_entry) {
  overrides <- list()

  for (key in names(entry)) {
    # var_name is always derivable from the column name -- skip it
    if (key == "var_name") next

    val <- entry[[key]]
    default_val <- default_entry[[key]]

    if (!identical_or_both_na(val, default_val)) {
      overrides[[key]] <- val
    }
  }

  if (length(overrides) == 0) return(NULL)
  overrides
}

box::export(
  build_default_config_entry,
  build_base_config,
  extract_overrides,
  identical_or_both_na
)
