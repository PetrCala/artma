box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    expect_false,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")

# Helper to create a mock data config
create_mock_data_config <- function(colnames) {
  config <- list()
  for (col in colnames) {
    col_clean <- make.names(col)
    config[[col_clean]] <- list(
      var_name = col,
      var_name_verbose = col,
      var_name_description = col,
      data_type = "float",
      group_category = NA,
      na_handling = NA,
      variable_summary = TRUE,
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA,
      bma = NA,
      bma_reference_var = NA,
      bma_to_log = NA,
      bpe = NA,
      bpe_sum_stats = NA,
      bpe_equal = NA,
      bpe_gltl = NA
    )
  }
  config
}

# Helper to create a simple test dataframe
create_test_df <- function() {
  data.frame(
    study = c("A", "B", "C"),
    effect = c(1.0, 2.0, 3.0),
    se = c(0.1, 0.2, 0.3),
    n_obs = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
}

test_that("remove_redundant_columns removes empty columns", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()
  df$empty_col <- NA
  df$another_empty <- rep(NA, nrow(df))

  # Create config with only the original columns
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1,
    "artma.data.extra_columns_strategy" = "keep"
  ))

  result <- remove_redundant_columns(df)

  expect_false("empty_col" %in% colnames(result))
  expect_false("another_empty" %in% colnames(result))
  expect_true("study" %in% colnames(result))
  expect_true("effect" %in% colnames(result))
})

test_that("remove_redundant_columns keeps columns with data when strategy is 'keep'", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()
  df$Notes <- c("Note 1", "Note 2", "Note 3")

  # Create config without Notes column
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1,
    "artma.data.extra_columns_strategy" = "keep"
  ))

  # Mock update_data_config to avoid actual file writes
  original_update <- getOption("artma.data.config")
  result <- remove_redundant_columns(df)

  expect_true("Notes" %in% colnames(result))
  expect_equal(result$Notes, df$Notes)
})

test_that("remove_redundant_columns removes columns with data when strategy is 'remove'", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()
  df$extra_col <- c(10, 20, 30)

  # Create config without extra_col
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 2,
    "artma.data.extra_columns_strategy" = "remove"
  ))

  result <- remove_redundant_columns(df)

  expect_false("extra_col" %in% colnames(result))
  expect_true("study" %in% colnames(result))
})

test_that("remove_redundant_columns aborts when strategy is 'abort'", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()
  df$extra_col <- c(10, 20, 30)

  # Create config without extra_col
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1,
    "artma.data.extra_columns_strategy" = "abort"
  ))

  expect_error(
    remove_redundant_columns(df),
    "Found.*extra column.*with data"
  )
})

test_that("remove_redundant_columns handles no redundant columns", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()

  # Create config with all columns
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  result <- remove_redundant_columns(df)

  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), colnames(df))
})

test_that("remove_redundant_columns handles mixed empty and data columns", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()
  df$empty_col <- NA
  df$data_col <- c(1, 2, 3)

  # Create config with only original columns
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1,
    "artma.data.extra_columns_strategy" = "keep"
  ))

  result <- remove_redundant_columns(df)

  # Empty column should be removed
  expect_false("empty_col" %in% colnames(result))
  # Data column should be kept (when strategy is keep)
  expect_true("data_col" %in% colnames(result))
})

test_that("remove_redundant_columns uses name-based comparison, not position", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  # Create df with columns in different order than config
  df <- data.frame(
    extra_col = c(1, 2, 3),
    study = c("A", "B", "C"),
    effect = c(1.0, 2.0, 3.0),
    se = c(0.1, 0.2, 0.3),
    n_obs = c(100, 200, 300),
    stringsAsFactors = FALSE
  )

  # Config expects study, effect, se, n_obs (no extra_col)
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1,
    "artma.data.extra_columns_strategy" = "remove"
  ))

  result <- remove_redundant_columns(df)

  # Should remove extra_col even though it's first, not last
  expect_false("extra_col" %in% colnames(result))
  expect_true("study" %in% colnames(result))
  expect_true("effect" %in% colnames(result))
})

test_that("remove_redundant_columns handles make.names standardization", {
  box::use(artma / data / preprocess[remove_redundant_columns])

  df <- create_test_df()
  df$`Column With Spaces` <- c(1, 2, 3)

  # Config uses make.names version
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs", "Column.With.Spaces"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  # Should not remove the column since it matches after make.names
  result <- remove_redundant_columns(df)

  expect_true("Column.With.Spaces" %in% colnames(result))
})
