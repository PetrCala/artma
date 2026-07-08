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

# Tests for remove_redundant_columns, verify_variable_names, and
# handle_extra_columns_with_data have been removed. Those functions were
# deleted and their responsibilities moved to inst/artma/data/schema_reconcile.R.
# See tests/testthat/test-data-schema-reconcile.R for the replacement tests.

# <U+2500><U+2500> enforce_data_types <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("enforce_data_types coerces columns according to the config", {
  box::use(artma / data / preprocess[enforce_data_types])

  # With no dataframe source, get_data_config returns the overrides as-is
  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.config" = list(
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
    "artma.data.config" = list(
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
    "artma.data.config" = list(
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
