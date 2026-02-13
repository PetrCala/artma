# nolint start

#' @keywords internal
test <- function() {
  # Local methods
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / testing / mocks / mock_df[create_mock_df],
    artma / data_config / parse[parse_df_into_data_config],
    artma / libs / core / utils[get_verbosity]
  )

  # mock_df_path <- PATHS$FILE_MOCKS_TMP_DATA
  mock_df_path <- "~/code/meta/artma/local/data/data_set_master_thesis_cala.csv"
  # mock_df <- create_mock_df(
  #   with_file_creation = TRUE,
  #   file_path = mock_df_path
  # )

  artma::artma(
    # options = "bachelor-thesis.yaml",
    # methods = c("variable_summary_stats", "p_hacking_tests")
  )
}

test()
# nolint end
