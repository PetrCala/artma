#' @title Prepare data
#' @description Prepare data for analysis. This includes reading, preprocessing, cleaning, and validating the data.
#' @return *\[data.frame\]* The prepared data frame.
prepare_data <- function() {
  cli::cli_inform("Preparing data for analysis.")

  box::use(
    artma / data / read[read_data],
    artma / data / preprocess[preprocess_data],
    artma / data / compute[compute_optional_columns]
  )


  df <- read_data()
  df <- preprocess_data(df)
  # df <- clean_data(df)
  # validate_data(df)
  df <- compute_optional_columns(df)
  # create_artifacts(df)
  df
}

box::export(
  prepare_data,
  preprocess_data,
  read_data
)
