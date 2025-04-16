box::use(
  testthat[test_that, expect_equal, expect_true],
  artma / const[CONST],
  artma / data_config / parse[construct_data_config_filename],
  artma / data_config / validate[data_config_filename_is_valid]
)

test_that("construct_data_config_filename returns valid filenames", {
  pattern <- CONST$PATTERNS$DATA_CONFIG$PLAIN
  df_names <- c(
    "dataset1",
    "experiment_data",
    "study_results_2023",
    "meta_analysis_output",
    "combined_data"
  )

  for (df_name in df_names) {
    expected_filename <- paste(df_name, pattern, sep = "")
    data_config_filename <- construct_data_config_filename(df_name)
    expect_equal(data_config_filename, expected_filename)
    expect_true(data_config_filename_is_valid(data_config_filename))
  }
})
