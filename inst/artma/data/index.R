box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

prepare_data_impl <- function() {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Preparing data for analysis.")
  }

  box::use(
    artma / data / read[read_data],
    artma / data / preprocess[preprocess_data],
    artma / data / compute[compute_optional_columns]
  )

  df <- read_data()
  df <- preprocess_data(df)
  df <- compute_optional_columns(df)

  df
}

#' @title Prepare data
#' @description Prepare data for analysis. This includes reading, preprocessing, cleaning, and validating the data.
#' @return *[data.frame]* The prepared data frame.
prepare_data <- cache_cli_runner(
  prepare_data_impl,
  stage = "prepare_data",
  key_builder = function(...) build_data_cache_signature()
)


# Re-export useful functions for external use
box::use(
  artma / data / read[read_data],
  artma / data / column_recognition[
    recognize_columns,
    get_required_column_names
  ],
  artma / data / interactive_mapping[
    column_mapping_workflow,
    interactive_column_mapping
  ],
  artma / data / smart_detection[
    detect_delimiter,
    smart_read_csv
  ]
)

box::export(
  prepare_data,
  read_data,
  recognize_columns,
  get_required_column_names,
  column_mapping_workflow,
  interactive_column_mapping,
  detect_delimiter,
  smart_read_csv
)
