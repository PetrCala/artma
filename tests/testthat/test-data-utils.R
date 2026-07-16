box::use(
  testthat[expect_error, expect_true, test_that]
)

box::use(
  testing / mocks / index[MOCKS],
  testing / fixtures / index[FIXTURES]
)

# Note: These tests use auto_detect = FALSE to test core logic without interaction.
# When auto_detect = TRUE, the new confirmation flow is triggered which requires
# user interaction via climenu. Those tests are in E2E tests (tests/E2E/).

test_that("standardize_column_names handles missing required columns in options correctly", {
  box::use(
    artma / data / utils[get_required_colnames, standardize_column_names]
  )

  mock_colnames <- MOCKS$create_mock_options_colnames()

  all_colnames <- names(mock_colnames)
  required_colnames <- get_required_colnames()
  non_required_colnames <- setdiff(all_colnames, required_colnames)

  # Build the full option list for a scenario, clearing (NULL) every colnames
  # option that the scenario omits so that state set by a previous scenario
  # cannot leak into the next one.
  build_scenario_options <- function(scenario_colnames) {
    opts <- lapply(stats::setNames(all_colnames, all_colnames), function(col) {
      scenario_colnames[[col]]
    })
    stats::setNames(opts, paste0("artma.data.colnames.", all_colnames))
  }

  # Enumerate every required and non-required column explicitly instead of
  # sampling an arbitrary one, so each run covers the full space.
  scenarios <- list()
  for (required_colname in required_colnames) {
    scenarios[[length(scenarios) + 1]] <- list(
      name = paste("missing required column:", required_colname),
      mock_colnames = mock_colnames[names(mock_colnames) != required_colname],
      expected_error = "Missing mapping for required columns:"
    )
  }
  for (non_required_colname in non_required_colnames) {
    scenarios[[length(scenarios) + 1]] <- list(
      name = paste("missing non-required column:", non_required_colname),
      mock_colnames = mock_colnames[names(mock_colnames) != non_required_colname],
      expected_error = NA
    )
  }
  scenarios[[length(scenarios) + 1]] <- list(
    name = "all missing columns",
    mock_colnames = lapply(mock_colnames, function(x) NULL),
    expected_error = "Missing mapping for required columns:"
  )
  scenarios[[length(scenarios) + 1]] <- list(
    name = "all columns present",
    mock_colnames = mock_colnames,
    expected_error = NA
  )

  for (scenario in scenarios) {
    mock_df <- MOCKS$create_mock_df(colnames_map = scenario$mock_colnames)
    withr::with_options(build_scenario_options(scenario$mock_colnames), {
      expect_error(
        standardize_column_names(df = mock_df, auto_detect = FALSE),
        scenario$expected_error,
        info = paste("Scenario:", scenario$name)
      )
    })
  }
})

test_that("standardize_column_names handles missing required columns in data correctly", {
  box::use(
    artma / data / utils[get_required_colnames, standardize_column_names]
  )

  required_colnames <- get_required_colnames()

  # Enumerate every required column explicitly instead of sampling one, so
  # each run covers the full space.
  scenarios <- list()
  for (required_colname in required_colnames) {
    scenarios[[length(scenarios) + 1]] <- list(
      name = paste("missing required column:", required_colname),
      missing_colnames = required_colname,
      expected_error = "These required columns are absent in the data frame"
    )
  }
  scenarios[[length(scenarios) + 1]] <- list(
    name = "all required columns missing",
    missing_colnames = required_colnames,
    expected_error = "These required columns are absent in the data frame"
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
