box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    expect_false,
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

test_that("enforce_correct_values treats an NA option as remove", {
  box::use(artma / data / preprocess[enforce_correct_values])

  # Options with a `.na` template default are loaded into options() as a
  # literal NA; the read must fall back to "remove" instead of crashing on
  # `if (NA == "stop")` (issue #321, bug 1).
  withr::local_options(list(
    "artma.calc.se_zero_handling" = NA,
    "artma.verbose" = 3
  ))

  df <- data.frame(se = c(0.1, 0, 0.2))
  expect_warning(result <- enforce_correct_values(df), "Removed 1 row")

  expect_equal(result$se, c(0.1, 0.2))
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
  expect_error(enforce_correct_values(df), "contains zero values in 1 row")
  # The abort names the option that decides the policy (#540).
  expect_error(enforce_correct_values(df), "se_zero_handling")
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

# resolve_se_zero_handling now lives in inst/artma/data/configure.R; see
# tests/testthat/test-data-configure.R for its tests.

# -- winsorize_data ------------------------------------------------------------

test_that("winsorize_data clips to order statistics, not interpolated quantiles", {
  box::use(artma / data / preprocess[winsorize_data])

  withr::local_options(list(
    "artma.data.winsorization_level" = 0.1,
    "artma.verbose" = 1
  ))

  # With 20 observations and p = 0.1, the type 1 (inverse ECDF) quantiles are
  # the 2nd and 18th order statistics. Type 7 would interpolate to 2.9 and
  # 18.1, values no observation attains.
  df <- data.frame(effect = 1:20 / 1, se = 20:1 / 10)
  result <- winsorize_data(df)

  expect_equal(min(result$effect), 2)
  expect_equal(max(result$effect), 18)
  expect_equal(result$effect, pmax(pmin(df$effect, 18), 2))
  expect_true(all(result$effect %in% df$effect))

  expect_equal(min(result$se), 0.2)
  expect_equal(max(result$se), 1.8)
  expect_equal(result$se, pmax(pmin(df$se, 1.8), 0.2))
  expect_true(all(result$se %in% df$se))
})

test_that("winsorize_data ignores NA values when locating the clip points", {
  box::use(artma / data / preprocess[winsorize_data])

  withr::local_options(list(
    "artma.data.winsorization_level" = 0.1,
    "artma.verbose" = 1
  ))

  df <- data.frame(effect = c(NA, 1:20, NA), se = rep(0.5, 22))
  result <- winsorize_data(df)

  expect_true(all(is.na(result$effect[c(1, 22)])))
  expect_equal(result$effect[2:21], pmax(pmin(1:20, 18), 2))
  expect_equal(result$se, df$se)
})

test_that("winsorize_data is a no-op when the level is zero", {
  box::use(artma / data / preprocess[winsorize_data])

  withr::local_options(list(
    "artma.data.winsorization_level" = 0,
    "artma.verbose" = 1
  ))

  df <- data.frame(effect = 1:20 / 1, se = 20:1 / 10)
  expect_equal(winsorize_data(df), df)
})

# -- numeric roles -------------------------------------------------------------

test_that("enforce_data_types coerces a numeric role typed as category to numeric", {
  box::use(artma / data / preprocess[enforce_data_types])

  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.columns" = list(
      effect = list(var_name = "effect", data_type = "category"),
      se = list(var_name = "se", data_type = "category"),
      label = list(var_name = "label", data_type = "category")
    ),
    "artma.verbose" = 1
  ))

  df <- data.frame(
    effect = c("0.817", NA, "-9.846"),
    se = factor(c("0.1", "0.2", "0.3")),
    label = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  result <- enforce_data_types(df)

  expect_equal(result$effect, c(0.817, NA, -9.846))
  expect_equal(result$se, c(0.1, 0.2, 0.3))
  expect_true(is.character(result$label))
})

test_that("enforce_data_types aborts on a numeric role that does not parse, naming the source column", {
  box::use(artma / data / preprocess[enforce_data_types])

  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.columns" = list(
      effect = list(var_name = "effect", source_name = "beta_estimate", data_type = "category"),
      se = list(var_name = "se", data_type = "float")
    ),
    "artma.verbose" = 1
  ))

  df <- data.frame(
    effect = c("0,817", "0,794", "-9,846", "1,023"),
    se = c("0.1", "0.2", "0.3", "0.4"),
    stringsAsFactors = FALSE
  )

  err <- expect_error(enforce_data_types(df), class = "rlang_error")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_true(grepl('"effect"', msg, fixed = TRUE))
  expect_true(grepl('mapped from "beta_estimate"', msg, fixed = TRUE))
  expect_true(grepl("0,817", msg, fixed = TRUE))
  expect_true(grepl("comma decimal separators", msg, fixed = TRUE))
})

test_that("enforce_data_types does not blame a decimal comma for arbitrary text", {
  box::use(artma / data / preprocess[enforce_data_types])

  withr::local_options(list(
    "artma.data.source_path" = NA,
    "artma.data.columns" = list(
      n_obs = list(var_name = "n_obs", data_type = "category")
    ),
    "artma.verbose" = 1
  ))

  df <- data.frame(n_obs = c("120", "n/a", "80 obs"), stringsAsFactors = FALSE)

  err <- expect_error(enforce_data_types(df), class = "rlang_error")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_true(grepl('"n_obs"', msg, fixed = TRUE))
  expect_true(grepl("n/a", msg, fixed = TRUE))
  expect_true(grepl("80 obs", msg, fixed = TRUE))
  expect_false(grepl("120", msg, fixed = TRUE))
  expect_false(grepl("comma decimal", msg, fixed = TRUE))
})

test_that("assert_numeric_roles passes numeric frames through unchanged", {
  box::use(artma / data / preprocess[assert_numeric_roles])

  withr::local_options(list("artma.data.columns" = list()))

  df <- data.frame(
    study_id = c("a", "b"),
    effect = c(1.5, 2.5),
    se = 1:2,
    n_obs = c(10L, 20L),
    moderator = c("x", "y"),
    stringsAsFactors = FALSE
  )
  expect_equal(assert_numeric_roles(df), df)
})

test_that("assert_numeric_roles aborts on a text numeric role", {
  box::use(artma / data / preprocess[assert_numeric_roles])

  withr::local_options(list("artma.data.columns" = list()))

  df <- data.frame(effect = c(1, 2), se = c("0.1", "0.2"), stringsAsFactors = FALSE)

  err <- expect_error(assert_numeric_roles(df), class = "rlang_error")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_true(grepl('"se"', msg, fixed = TRUE))
  expect_true(grepl("0.1", msg, fixed = TRUE))
  expect_false(grepl("mapped from", msg, fixed = TRUE))
})

test_that("winsorize_data refuses to clip a text column", {
  box::use(artma / data / preprocess[winsorize_data])

  withr::local_options(list(
    "artma.data.winsorization_level" = 0.1,
    "artma.data.columns" = list(),
    "artma.verbose" = 1
  ))

  df <- data.frame(
    effect = c("0,817", "0,057", "-9,846", "1,023"),
    se = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )

  err <- expect_error(winsorize_data(df), class = "rlang_error")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_true(grepl('"effect"', msg, fixed = TRUE))
  expect_true(grepl("is not numeric", msg, fixed = TRUE))
})
