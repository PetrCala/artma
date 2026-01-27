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

  # Config uses make.names version in var_name, but comparison is done after make.names
  # So "Column With Spaces" should match "Column.With.Spaces" from config
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs", "Column.With.Spaces"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  # Should not remove the column since it matches after make.names standardization
  # The column name in the dataframe remains "Column With Spaces" (original name)
  result <- remove_redundant_columns(df)

  expect_true("Column With Spaces" %in% colnames(result))
})

test_that("verify_variable_names accepts columns in any order", {
  box::use(artma / data / preprocess[verify_variable_names])

  # Create df with columns in different order than config
  df <- data.frame(
    n_obs = c(100, 200, 300),
    se = c(0.1, 0.2, 0.3),
    effect = c(1.0, 2.0, 3.0),
    study = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Config expects study, effect, se, n_obs (different order)
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  # Should pass validation regardless of order
  result <- verify_variable_names(df)

  expect_equal(ncol(result), ncol(df))
  expect_setequal(colnames(result), c("study", "effect", "se", "n_obs"))
})

test_that("verify_variable_names catches missing columns", {
  box::use(artma / data / preprocess[verify_variable_names])

  df <- create_test_df()
  # Remove one column
  df$se <- NULL

  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  expect_error(
    verify_variable_names(df),
    "Missing columns"
  )
})

test_that("verify_variable_names catches extra columns", {
  box::use(artma / data / preprocess[verify_variable_names])

  df <- create_test_df()
  df$extra_col <- c(1, 2, 3)

  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  expect_error(
    verify_variable_names(df),
    "Unexpected columns"
  )
})

test_that("verify_variable_names filters out NA values from config", {
  box::use(artma / data / preprocess[verify_variable_names])

  df <- create_test_df()

  # Create config with NA value
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))
  # Add an entry with NA var_name
  config[["invalid"]] <- list(
    var_name = NA,
    var_name_verbose = "Invalid",
    var_name_description = "Invalid",
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

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  # Should pass validation (NA is filtered out)
  result <- verify_variable_names(df)

  expect_equal(ncol(result), ncol(df))
  expect_setequal(colnames(result), c("study", "effect", "se", "n_obs"))
})

test_that("verify_variable_names provides clear error messages", {
  box::use(artma / data / preprocess[verify_variable_names])

  df <- create_test_df()
  df$extra_col <- c(1, 2, 3)
  df$se <- NULL # Remove required column

  config <- create_mock_data_config(c("study", "effect", "se", "n_obs"))

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  expect_error(
    verify_variable_names(df),
    "All expected non-computed columns must exist"
  )

  expect_error(
    verify_variable_names(df),
    "Missing required columns"
  )

  expect_error(
    verify_variable_names(df),
    "Unexpected columns"
  )
})

test_that("verify_variable_names allows computed columns to be missing", {
  box::use(artma / data / preprocess[verify_variable_names])

  df <- create_test_df()
  # df does NOT have t_stat, study_size, reg_dof, precision (computed columns)

  # Create config with both required and computed columns
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs", "t_stat", "study_size", "reg_dof", "precision"))

  # Mark computed columns as computed
  config[["t_stat"]]$is_computed <- TRUE
  config[["study_size"]]$is_computed <- TRUE
  config[["reg_dof"]]$is_computed <- TRUE
  config[["precision"]]$is_computed <- TRUE

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  # Should pass validation - computed columns are allowed to be missing
  result <- verify_variable_names(df)

  expect_equal(ncol(result), ncol(df))
  expect_setequal(colnames(result), c("study", "effect", "se", "n_obs"))
})

test_that("verify_variable_names still requires non-computed columns", {
  box::use(artma / data / preprocess[verify_variable_names])

  df <- create_test_df()
  df$se <- NULL # Remove required column

  # Create config with both required and computed columns
  config <- create_mock_data_config(c("study", "effect", "se", "n_obs", "t_stat"))

  # Mark t_stat as computed
  config[["t_stat"]]$is_computed <- TRUE

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.verbose" = 1
  ))

  # Should fail - se is required but missing
  expect_error(
    verify_variable_names(df),
    "Missing required columns"
  )
})
