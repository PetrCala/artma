#' Assign NA to a column in a data frame
#' @export
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  return(df)
}



#' Get the number of studies in an analysis data frame.
#'
#' @param df [data.frame] The analysis data frame.
#' @param study_colname [str] The column name holding names of all studies.
#' @return [int] The number of studies.
#' @export
get_number_of_studies <- function(df) {
  if (!"study" %in% colnames(df)) {
    rlang::abort("The data frame does not have a 'study' column.", class = "missing_study_column")
  }
  return(length(table(df$study)))
}
