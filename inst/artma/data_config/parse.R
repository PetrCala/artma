#' Parse a dataframe into a data config
#'
#' @param df *\[data.frame\]* The dataframe to parse
#' @return *\[list\]* The data config
parse_df_into_data_config <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data / utils[determine_vector_type],
    artma / libs / validation[validate],
    artma / libs / string[make_verbose_name]
  )

  validate(is.data.frame(df))

  if (nrow(df) == 0) {
    cli::cli_abort("The dataframe is empty. Please provide a dataframe with at least one row.")
  }

  config <- list()

  for (col in names(df)) {
    col_config <- list()
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

    KEYS <- CONST$DATA_CONFIG$KEYS
    col_config[[KEYS$VAR_NAME]] <- col
    col_config[[KEYS$VAR_NAME_VERBOSE]] <- col_name_verbose
    col_config[[KEYS$VAR_NAME_DESCRIPTION]] <- col_name_verbose
    col_config[[KEYS$DATA_TYPE]] <- col_data_type
    col_config[[KEYS$GROUP_CATEGORY]] <- NA # 1, 1, 2, 3, ...
    col_config[[KEYS$NA_HANDLING]] <- getOption(
      "artma.data.na_handling"
    )
    col_config[[KEYS$VARIABLE_SUMMARY]] <- NA
    col_config[[KEYS$EFFECT_SUM_STATS]] <- NA
    col_config[[KEYS$EQUAL]] <- NA
    col_config[[KEYS$GLTL]] <- NA
    col_config[[KEYS$BMA]] <- NA
    col_config[[KEYS$BMA_REFERENCE_VAR]] <- NA
    col_config[[KEYS$BMA_TO_LOG]] <- NA
    col_config[[KEYS$BPE]] <- NA
    col_config[[KEYS$BPE_SUM_STATS]] <- NA
    col_config[[KEYS$BPE_EQUAL]] <- NA
    col_config[[KEYS$BPE_GLTL]] <- NA

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
