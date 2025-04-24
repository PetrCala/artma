test_that("standardize_column_names handles missing columns correctly", {
  box::use(
    artma / libs / validation[assert],
    artma / data / preprocess[standardize_column_names],
    artma / data / utils[get_required_colnames]
  )

  mock_colnames <- MOCKS$create_mock_options_colnames()

  all_colnames <- names(mock_colnames)
  required_colnames <- get_required_colnames()
  arbitrary_required_colname <- sample(required_colnames, 1)
  arbitrary_non_required_colname <- sample(setdiff(all_colnames, required_colnames), 1)

  assert(arbitrary_required_colname %in% required_colnames)
  assert(!arbitrary_non_required_colname %in% required_colnames)

  colnames_with_one_required_missing <- mock_colnames[-which(names(mock_colnames) == arbitrary_required_colname)]
  colnames_with_one_non_required_missing <- mock_colnames[-which(names(mock_colnames) == arbitrary_non_required_colname)]

  scenarios <- list(
    list(
      name = "one missing required column",
      mock_colnames = colnames_with_one_required_missing,
      expected_error = "Missing mapping for required columns:"
    ),
    list(
      name = "one missing non-required column",
      mock_colnames = colnames_with_one_non_required_missing,
      expected_error = NA
    ),
    list(
      name = "all missing columns",
      mock_colnames = list(),
      expected_error = "Missing mapping for required columns:"
    ),
    list(
      name = "all columns present",
      mock_colnames = mock_colnames,
      expected_error = NA
    )
  )

  for (scenario in scenarios) {
    mock_df <- MOCKS$create_mock_df(colnames_map = scenario$mock_colnames)
    expect_error(
      standardize_column_names(
        df = mock_df,
        map = scenario$mock_colnames
      ),
      scenario$expected_error,
      info = paste("Scenario:", scenario$name)
    )
  }
})
