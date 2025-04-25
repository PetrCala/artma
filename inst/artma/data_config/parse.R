#' Parse a dataframe into a data config
#'
#' @param df *\[data.frame\]* The dataframe to parse
#' @return *\[list\]* The data config
parse_df_into_data_config <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data / utils[determine_vector_type],
    artma / libs / validation[validate, assert],
    artma / libs / string[make_verbose_name]
  )

  validate(is.data.frame(df))

  if (nrow(df) == 0) {
    cli::cli_abort("The dataframe is empty. Please provide a dataframe with at least one row.")
  }

  config <- list()

  for (col in names(df)) {
    col_name_clean <- make.names(col)
    col_name_verbose <- make_verbose_name(col)
    col_data <- df[[col]]

    col_data_type <- tryCatch(
      determine_vector_type(
        data = col_data,
        recognized_data_types = CONST$DATA_CONFIG$DATA_TYPES
      ),
      error = function(e) {
        cli::cli_alert_warning("Failed to determine the data type of the column {.val {col}}.")
        "unknown"
      }
    )

    col_config <- list(
      "var_name" = col,
      "var_name_verbose" = col_name_verbose,
      "var_name_description" = col_name_verbose,
      "data_type" = col_data_type,
      "group_category" = NA,
      "na_handling" = getOption(
        "artma.data.na_handling"
      ),
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

    assert(
      all(names(col_config) %in% CONST$DATA_CONFIG$KEYS),
      msg = "The column {.val {col}} has an invalid data config key."
    )

    config[[col_name_clean]] <- col_config
  }

  # column_configs <- lapply(names(df), process_column, df = df)

  # config <- stats::setNames(
  #   lapply(column_configs, function(x) x$config),
  #   vapply(column_configs, function(x) x$name, character(1))
  # )

  config
}

box::export(
  parse_df_into_data_config
)
