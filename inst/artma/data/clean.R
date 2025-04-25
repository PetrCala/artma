box::use(
  artma / data / fill[fill_missing_values]
)

#' Check that a data frame contains all the expected columns
#'
#' @param df *\[data.frame\]* The data frame to check
#' @param expected_cols *\[character\]* The list of expected column names
#' @example
#' check_for_missing_cols(df, c("Effect", "Standard Error", "Lower CI", "Upper CI"))
#' # Throws an error if any of the columns are missing
check_for_missing_cols <- function(df, expected_cols) {
  missing_cols <- setdiff(expected_cols, colnames(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      paste("The data frame is missing the following columns:", missing_cols),
      class = "missing_columns_error"
    )
  }
}

#' Convert selected columns to numeric
convert_columns_to_numeric <- function(df, cols) {
  cli::cli_inform("Converting the following columns to numeric values: {.emph {cols}}")
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    } else {
      cli::cli_alert_warning(paste("Column", col, "does not exist in the dataframe"))
    }
  }
  return(df)
}


#' Clean a data frame for analysis
#'
#' @param df *\[data.frame\]* The data frame to clean
#' @param analysis_name *\[character\]* The name of the analysis
#' @param clean_names *\[logical\]* Whether to clean the names of the studies and files. Defaults to TRUE
#' @param recalculate_t_value *\[logical\]* Whether to recalculate the t-value based on the effect and se columns. Defaults to TRUE
#' @param fill_dof *\[logical\]* Whether to fill missing degrees of freedom using the PCC method. Defaults to TRUE
clean_data <- function(
    df,
    analysis_name,
    clean_names = TRUE,
    recalculate_t_value = TRUE,
    fill_dof = TRUE) {
  cli::cli_inform("Cleaning data...")
  cli::cli_abort("NOT IMPLEMENTED")
  source_cols <- c("a", "b", "c")

  # Subset to relevant colnames - use colname if available, column source if not
  get_colname <- function(col) source_cols[[col]] %||% col
  relevant_colnames <- unlist(lapply(names(source_cols), get_colname))
  check_for_missing_cols(df, relevant_colnames) # Validate cols are present before subsetting
  df <- df[, relevant_colnames]

  # Rename the columns
  colnames(df) <- names(source_cols)

  # Ensure numeric values
  df <- convert_columns_to_numeric(df, cols = c("effect", "se", "sample_size", "dof"))

  # Fill missing studies
  df <- fill_missing_values(df = df, target_col = "study", columns = c("author1", "year"), missing_value_prefix = "Missing study")

  if (clean_names) {
    df <- clean_names(df = df) # Clean names of studies and files
  }

  if (recalculate_t_value) {
    df <- recalculate_t_value(df = df) # Recalculate t-values
  }

  cli::cli_inform("Rows after data cleaning: {.val {nrow(df)}}")

  return(df)
}

box::export(
  check_for_missing_cols,
  clean_data
)
