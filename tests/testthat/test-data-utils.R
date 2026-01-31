box::use(
  artma / testing / mocks / index[MOCKS],
  artma / testing / fixtures / index[FIXTURES]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")

# Note: These tests use auto_detect = FALSE to test core logic without interaction.
# When auto_detect = TRUE, the new confirmation flow is triggered which requires
# user interaction via climenu. Those tests are in E2E tests (tests/E2E/).

test_that("standardize_column_names handles missing required columns in options correctly", {
  box::use(
    artma / libs / core / validation[assert],
    artma / data / utils[get_required_colnames, standardize_column_names]
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
      mock_colnames = lapply(mock_colnames, function(x) NULL),
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
    FIXTURES$with_custom_colnames(scenario$mock_colnames)
    expect_error(
      standardize_column_names(df = mock_df, auto_detect = FALSE),
      scenario$expected_error,
      info = paste("Scenario:", scenario$name)
    )
  }
})

test_that("standardize_column_names handles missing required columns in data correctly", {
  box::use(
    artma / data / utils[get_required_colnames, standardize_column_names]
  )

  required_colnames <- get_required_colnames()

  scenarios <- list(
    list(
      name = "one missing required column",
      missing_colnames = setdiff(required_colnames, sample(required_colnames, 1)),
      expected_error = "These required columns are absent in the data frame"
    ),
    list(
      name = "more missing non-required columns",
      missing_colnames = setdiff(required_colnames, sample(required_colnames, 2)),
      expected_error = "These required columns are absent in the data frame"
    ),
    list(
      name = "all missing columns",
      missing_colnames = required_colnames,
      expected_error = "These required columns are absent in the data frame"
    )
  )

  for (scenario in scenarios) {
    mock_colnames <- MOCKS$create_mock_options_colnames()
    FIXTURES$with_custom_colnames(mock_colnames)
    mock_df <- MOCKS$create_mock_df(colnames_map = mock_colnames)
    mock_df <- mock_df[, -which(names(mock_df) %in% scenario$missing_colnames), drop = FALSE]

    expect_error(
      standardize_column_names(mock_df, auto_detect = FALSE),
      scenario$expected_error,
      info = paste("Scenario:", scenario$name)
    )
  }
})

test_that("standardize_column_names standardizes non-standard column names", {
  box::use(artma / data / utils[standardize_column_names])

  non_standard_name <- make.names("non-standard-study-column-name")
  mock_colnames <- MOCKS$create_mock_options_colnames(
    colnames = list(
      "study_id" = non_standard_name
    )
  )
  FIXTURES$with_custom_colnames(mock_colnames)
  mock_df <- MOCKS$create_mock_df(colnames_map = mock_colnames)
  expect_true(non_standard_name %in% colnames(mock_df))
  expect_true(!"study_id" %in% colnames(mock_df))

  standardized_df <- standardize_column_names(mock_df, auto_detect = FALSE)
  expect_true(!non_standard_name %in% colnames(standardized_df))
  expect_true("study_id" %in% colnames(standardized_df))
})

test_that("standardize_column_names passes when all required columns are present", {
  box::use(artma / data / utils[standardize_column_names])

  mock_colnames <- MOCKS$create_mock_options_colnames()
  FIXTURES$with_custom_colnames(mock_colnames)
  mock_df <- MOCKS$create_mock_df(colnames_map = mock_colnames)

  expect_error(
    standardize_column_names(mock_df, auto_detect = FALSE),
    NA,
    info = "Standardizing column names should pass when all required columns are present"
  )
})
