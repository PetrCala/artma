box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    expect_false,
    expect_null,
    expect_warning,
    expect_no_warning,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")
expect_null <- getFromNamespace("expect_null", "testthat")
expect_warning <- getFromNamespace("expect_warning", "testthat")
expect_no_warning <- getFromNamespace("expect_no_warning", "testthat")

# Tests for remove_redundant_columns, verify_variable_names, and
# handle_extra_columns_with_data have been removed. Those functions were
# deleted and their responsibilities moved to inst/artma/data/schema_reconcile.R.
# See tests/testthat/test-data-schema-reconcile.R for the replacement tests.

# -- enforce_data_types --------------------------------------------------------

test_that("enforce_data_types coerces columns according to the config", {
  box::use(artma / data / preprocess[enforce_data_types])

  # With no dataframe source, get_data_config returns the overrides as-is
  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.columns" = list(
      x = list(var_name = "x", data_type = "int"),
      y = list(var_name = "y", data_type = "category"),
      z = list(var_name = "z", data_type = "float")
    ),
    "artma.verbose" = 1
  ))

  df <- data.frame(x = c(1, 2), y = c(TRUE, FALSE), z = c("1.5", "2.5"))
  result <- enforce_data_types(df)

  expect_true(is.integer(result$x))
  expect_true(is.character(result$y))
  expect_true(is.numeric(result$z))
})

test_that("enforce_data_types errors on a column with no config entry", {
  box::use(artma / data / preprocess[enforce_data_types])

  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.columns" = list(
      x = list(var_name = "x", data_type = "float")
    ),
    "artma.verbose" = 1
  ))

  df <- data.frame(x = c(1.5, 2.5), y = c("a", "b"))

  expect_error(enforce_data_types(df), "No data config entry")
})

test_that("enforce_data_types skips entries without type information", {
  box::use(artma / data / preprocess[enforce_data_types])

  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.columns" = list(
      x = list(var_name = "x", data_type = "float"),
      y = list(var_name = "y")
    ),
    "artma.verbose" = 1
  ))

  df <- data.frame(x = c(1.5, 2.5), y = c("a", "b"))
  result <- enforce_data_types(df)

  expect_true(is.numeric(result$x))
  expect_equal(result$y, df$y)
})

# -- apply_subset_conditions ---------------------------------------------------

test_that("apply_subset_conditions returns the data frame unchanged when unset", {
  box::use(artma / data / preprocess[apply_subset_conditions])

  withr::local_options(list(
    "artma.data.subset_conditions" = NA_character_,
    "artma.verbose" = 1
  ))

  df <- data.frame(country = c("USA", "UK"), year = c(1999, 2001))
  expect_equal(apply_subset_conditions(df), df)
})

test_that("apply_subset_conditions filters rows matching a single condition", {
  box::use(artma / data / preprocess[apply_subset_conditions])

  withr::local_options(list(
    "artma.data.subset_conditions" = "year >= 2000",
    "artma.verbose" = 1
  ))

  df <- data.frame(country = c("USA", "UK", "USA"), year = c(1999, 2001, 2005))
  result <- apply_subset_conditions(df)

  expect_equal(result$year, c(2001, 2005))
})

test_that("apply_subset_conditions combines multiple conditions with AND", {
  box::use(artma / data / preprocess[apply_subset_conditions])

  withr::local_options(list(
    "artma.data.subset_conditions" = c("year >= 2000", "country == 'USA'"),
    "artma.verbose" = 1
  ))

  df <- data.frame(
    country = c("USA", "UK", "USA"),
    year = c(1999, 2001, 2005)
  )
  result <- apply_subset_conditions(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$year, 2005)
})

test_that("apply_subset_conditions drops rows where the condition evaluates to NA", {
  box::use(artma / data / preprocess[apply_subset_conditions])

  withr::local_options(list(
    "artma.data.subset_conditions" = "year >= 2000",
    "artma.verbose" = 1
  ))

  df <- data.frame(country = c("USA", "UK"), year = c(NA, 2001))
  result <- apply_subset_conditions(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$country, "UK")
})

test_that("apply_subset_conditions errors on an unparseable condition", {
  box::use(artma / data / preprocess[apply_subset_conditions])

  withr::local_options(list(
    "artma.data.subset_conditions" = "year >=",
    "artma.verbose" = 1
  ))

  df <- data.frame(year = c(1999, 2001))
  expect_error(apply_subset_conditions(df), "Invalid subset condition")
})

test_that("apply_subset_conditions errors when a condition targets a missing column", {
  box::use(artma / data / preprocess[apply_subset_conditions])

  withr::local_options(list(
    "artma.data.subset_conditions" = "region == 'EU'",
    "artma.verbose" = 1
  ))

  df <- data.frame(country = c("USA", "UK"), year = c(1999, 2001))
  expect_error(apply_subset_conditions(df), "Failed to evaluate subset condition")
})

# -- enforce_correct_values ------------------------------------------------------

test_that("enforce_correct_values defaults to removing rows with zero SE when unset", {
  box::use(artma / data / preprocess[enforce_correct_values])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = NULL,
    "artma.verbose" = 3
  ))

  df <- data.frame(se = c(0.1, 0, 0.2, 0))
  expect_warning(result <- enforce_correct_values(df), "Removed 2 rows")

  expect_equal(nrow(result), 2)
  expect_true(all(result$se != 0))
})

test_that("enforce_correct_values 'remove' strategy drops zero-SE rows with a warning", {
  box::use(artma / data / preprocess[enforce_correct_values])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "remove",
    "artma.verbose" = 3
  ))

  df <- data.frame(se = c(0.1, 0, 0.2))
  expect_warning(result <- enforce_correct_values(df), "stricter validation")

  expect_equal(result$se, c(0.1, 0.2))
})

test_that("enforce_correct_values 'remove' strategy is a no-op without zero SE", {
  box::use(artma / data / preprocess[enforce_correct_values])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "remove",
    "artma.verbose" = 1
  ))

  df <- data.frame(se = c(0.1, 0.2))
  result <- expect_no_warning(enforce_correct_values(df))

  expect_equal(result, df)
})

test_that("enforce_correct_values 'stop' strategy aborts on zero SE", {
  box::use(artma / data / preprocess[enforce_correct_values])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "stop",
    "artma.verbose" = 1
  ))

  df <- data.frame(se = c(0.1, 0))
  expect_error(enforce_correct_values(df), "contains zero values")
})

test_that("enforce_correct_values 'warn' strategy keeps rows but warns", {
  box::use(artma / data / preprocess[enforce_correct_values])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "warn",
    "artma.verbose" = 3
  ))

  df <- data.frame(se = c(0.1, 0))
  expect_warning(result <- enforce_correct_values(df), "contains zero values")

  expect_equal(result, df)
})

test_that("enforce_correct_values 'ignore' strategy silently keeps rows", {
  box::use(artma / data / preprocess[enforce_correct_values])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "ignore",
    "artma.verbose" = 3
  ))

  df <- data.frame(se = c(0.1, 0))
  result <- expect_no_warning(enforce_correct_values(df))

  expect_equal(result, df)
})

# -- resolve_se_zero_handling ------------------------------------------------------

test_that("resolve_se_zero_handling is a no-op when the option is already configured", {
  box::use(artma / data / preprocess[resolve_se_zero_handling])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "stop",
    "artma.verbose" = 1
  ))

  df <- data.frame(se = c(0.1, 0))
  resolve_se_zero_handling(df)

  expect_equal(getOption("artma.calc.se_zero_handling"), "stop")
})

test_that("resolve_se_zero_handling is a no-op when no zero SE rows are found", {
  box::use(artma / data / preprocess[resolve_se_zero_handling])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = NULL,
    "artma.verbose" = 1
  ))

  df <- data.frame(se = c(0.1, 0.2))
  resolve_se_zero_handling(df)

  expect_null(getOption("artma.calc.se_zero_handling"))
})

test_that("resolve_se_zero_handling defaults to 'remove' in a non-interactive session", {
  box::use(artma / data / preprocess[resolve_se_zero_handling])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = NULL,
    "artma.autonomy.level" = NULL,
    "artma.verbose" = 2
  ))

  df <- data.frame(se = c(0.1, 0, 0.2))
  expect_warning(resolve_se_zero_handling(df), "stricter validation")

  expect_equal(getOption("artma.calc.se_zero_handling"), "remove")
})
