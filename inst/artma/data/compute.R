#' @title Compute optional columns
#' @description Compute optional columns that the user did not provide.
#' @param df *\[data.frame\]* The data frame to compute the optional columns for.
#' @return *\[data.frame\]* The data frame with the optional columns.
compute_optional_columns <- function(df) {
  box::use(artma / options / utils[get_option_group])

  # colnames <- get_option_group("artma.data.colnames")

  df
}

box::export(compute_optional_columns)
