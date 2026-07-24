box::use(
  testthat[expect_equal, expect_error, expect_false, expect_true, test_that]
)

box::use(
  testing / mocks / index[MOCKS],
  testing / fixtures / index[FIXTURES]
)

# Note: standardize_column_names() is a pure standardizer with no interactive
# fallback. Interactive column mapping is a separate workflow (see
# artma / data / interactive_mapping) that callers run and persist before
# invoking this function; those tests live in tests/E2E/.

test_that("standardize_column_names handles missing required columns in options correctly", {
  box::use(
    artma / data / utils[get_required_colnames, standardize_column_names]
  )

  # Rename every standard column in the mock data, so a required column with no
  # record in the store cannot be silently resolved by the identity fallback
  # (a df column that already carries the standard name needs no mapping).
  renames <- list(
    obs_id = "obs_id_col", study_id = "study_id_col", effect = "effect_col",
    se = "se_col", t_stat = "t_stat_col", n_obs = "n_obs_col",
    study_size = "study_size_col", reg_dof = "reg_dof_col", precision = "precision_col"
  )
  mock_colnames <- MOCKS$create_mock_options_colnames(colnames = renames)
  mock_df <- MOCKS$create_mock_df(colnames_map = mock_colnames)

  all_colnames <- names(mock_colnames)
  required_colnames <- get_required_colnames()
  non_required_colnames <- setdiff(all_colnames, required_colnames)

  # Build the unified column store for a scenario: one role record with a
  # source_name per mapped column.
  build_scenario_options <- function(scenario_colnames) {
    records <- list()
    for (col in names(scenario_colnames)) {
      val <- scenario_colnames[[col]]
      if (is.null(val)) next
      records[[col]] <- list(source_name = val)
    }
    list("artma.data.columns" = records)
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
    withr::with_options(build_scenario_options(scenario$mock_colnames), {
      expect_error(
        standardize_column_names(df = mock_df),
        scenario$expected_error,
        info = paste("Scenario:", scenario$name)
      )
    })
  }
})

test_that("standardize_column_names accepts identity columns without a stored record", {
  box::use(artma / data / utils[standardize_column_names])

  # All required columns are already present under their standard names, so an
  # empty store is sufficient.
  mock_df <- MOCKS$create_mock_df()

  withr::with_options(list("artma.data.columns" = list()), {
    standardized_df <- standardize_column_names(mock_df)
    expect_true(all(c("study_id", "effect", "se", "n_obs") %in% colnames(standardized_df)))
  })
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
      standardize_column_names(mock_df),
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

  standardized_df <- standardize_column_names(mock_df)
  expect_true(!non_standard_name %in% colnames(standardized_df))
  expect_true("study_id" %in% colnames(standardized_df))
})

test_that("standardize_column_names passes when all required columns are present", {
  box::use(artma / data / utils[standardize_column_names])

  mock_colnames <- MOCKS$create_mock_options_colnames()
  FIXTURES$with_custom_colnames(mock_colnames)
  mock_df <- MOCKS$create_mock_df(colnames_map = mock_colnames)

  expect_error(
    standardize_column_names(mock_df),
    NA,
    info = "Standardizing column names should pass when all required columns are present"
  )
})

# -- Rename-target collision --
#
# Reproduces a real bug: a role mapping (e.g. study_id -> study_name) targets a
# standard name that the data frame already has under a *different* column.
# Renaming used to silently produce two columns named "study_id", so
# `df$study_id` returned whichever came first in column order regardless of
# the user's mapping. standardize_column_names() must abort instead.

#' Build a synthetic data frame with the required columns plus one extra
#' column that can collide with a rename target.
#' @keywords internal
build_collision_df <- function(extra_col_name, extra_col_values) {
  df <- data.frame(
    study_id = 1:5,
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.01, 0.02, 0.03, 0.04, 0.05),
    n_obs = c(50L, 60L, 70L, 80L, 90L)
  )
  df[[extra_col_name]] <- extra_col_values
  df
}

test_that("standardize_column_names aborts when a rename target is already occupied", {
  box::use(artma / data / utils[standardize_column_names])

  # study_id is mapped to study_name, but the data frame already has its own,
  # different study_id column.
  mock_df <- build_collision_df("study_name", paste("Study", 1:5))

  withr::with_options(
    list("artma.data.columns" = list(study_id = list(source_name = "study_name"))),
    {
      expect_error(
        standardize_column_names(mock_df),
        "already has a different column named"
      )
    }
  )
})

test_that("the collision abort message names the exact remediation commands", {
  box::use(artma / data / utils[standardize_column_names])

  mock_df <- build_collision_df("study_name", paste("Study", 1:5))

  withr::with_options(
    list("artma.data.columns" = list(study_id = list(source_name = "study_name"))),
    {
      err <- tryCatch(
        standardize_column_names(mock_df, auto_detect = FALSE),
        error = function(e) conditionMessage(e)
      )
      expect_true(grepl('config.set("study_id", drop_conflicting_raw = TRUE)', err, fixed = TRUE))
      expect_true(grepl('config.reset("study_id")', err, fixed = TRUE))
    }
  )
})

test_that("standardize_column_names drops the raw column when drop_conflicting_raw is set", {
  box::use(artma / data / utils[standardize_column_names])

  mock_df <- build_collision_df("study_name", paste("Study", 1:5))

  withr::with_options(
    list(
      "artma.data.columns" = list(
        study_id = list(source_name = "study_name", drop_conflicting_raw = TRUE)
      ),
      "artma.verbose" = 1L
    ),
    {
      standardized_df <- standardize_column_names(mock_df, auto_detect = FALSE)
      expect_equal(sum(colnames(standardized_df) == "study_id"), 1)
      expect_equal(standardized_df$study_id, paste("Study", 1:5))
      expect_false("study_name" %in% colnames(standardized_df))
    }
  )
})

test_that("standardize_column_names allows an identity mapping through quietly", {
  box::use(artma / data / utils[standardize_column_names])

  mock_df <- build_collision_df("study_size", 1:5)

  withr::with_options(
    list("artma.data.columns" = list(study_id = list(source_name = "study_id"))),
    {
      standardized_df <- standardize_column_names(mock_df)
      expect_equal(anyDuplicated(colnames(standardized_df)), 0)
      expect_true("study_id" %in% colnames(standardized_df))
    }
  )
})

test_that("standardize_column_names drops a byte-identical duplicate target column quietly", {
  box::use(artma / data / utils[standardize_column_names])

  # study_name is byte-identical to study_id, so the collision is safe to
  # resolve without user input: the stale duplicate is dropped.
  mock_df <- build_collision_df("study_name", 1:5)

  withr::with_options(
    list("artma.data.columns" = list(study_id = list(source_name = "study_name"))),
    {
      standardized_df <- standardize_column_names(mock_df)
      expect_equal(sum(colnames(standardized_df) == "study_id"), 1)
      expect_equal(standardized_df$study_id, 1:5)
    }
  )
})

test_that("standardize_column_names still performs normal renames without collision", {
  box::use(artma / data / utils[standardize_column_names])

  mock_colnames <- MOCKS$create_mock_options_colnames(
    colnames = list(study_id = "study_id_raw")
  )
  FIXTURES$with_custom_colnames(mock_colnames)
  mock_df <- MOCKS$create_mock_df(colnames_map = mock_colnames)

  standardized_df <- standardize_column_names(mock_df)
  expect_equal(anyDuplicated(colnames(standardized_df)), 0)
  expect_true("study_id" %in% colnames(standardized_df))
  expect_false("study_id_raw" %in% colnames(standardized_df))
})
