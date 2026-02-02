#' Parse a dataframe into a data config
#'
#' @param df *\[data.frame\]* The dataframe to parse
#' @return *\[list\]* The data config
parse_df_into_data_config <- function(df) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[assert],
    artma / data_config / defaults[build_base_config]
  )

  config <- build_base_config(df)

  # Validate that all keys conform to the expected DATA_CONFIG keys
  for (col_key in names(config)) {
    assert(
      all(names(config[[col_key]]) %in% CONST$DATA_CONFIG$KEYS),
      msg = "The column {.val {col_key}} has an invalid data config key."
    )
  }

  config
}

box::export(
  parse_df_into_data_config
)
