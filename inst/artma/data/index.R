box::use(
  artma / data / read[read_data]
)

#' @title Prepare data
#' @description Prepare data for analysis. This includes reading, preprocessing, cleaning, and validating the data.
#' @return [data.frame] The prepared data frame.
prepare_data <- function() {
  logger::log_info("Preparing data for analysis.")

  df <- read_data()
  # df <- preprocess_data(df)
  # df <- clean_data(df)
  # validate_data(df)
  # create_artifacts(df)
  df
}

box::export(
  prepare_data,
  read_data
)
