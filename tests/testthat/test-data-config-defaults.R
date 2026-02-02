box::use(
  testthat[
    expect_equal,
    expect_null,
    expect_true,
    expect_false,
    expect_error,
    test_that,
    expect_length
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_null <- getFromNamespace("expect_null", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_length <- getFromNamespace("expect_length", "testthat")

# ── identical_or_both_na ─────────────────────────────────────────────────────

test_that("identical_or_both_na: both NA returns TRUE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_true(identical_or_both_na(NA, NA))
})

test_that("identical_or_both_na: both NULL returns TRUE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_true(identical_or_both_na(NULL, NULL))
})

test_that("identical_or_both_na: identical values return TRUE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_true(identical_or_both_na(TRUE, TRUE))
  expect_true(identical_or_both_na(FALSE, FALSE))
  expect_true(identical_or_both_na("hello", "hello"))
  expect_true(identical_or_both_na(42, 42))
})

test_that("identical_or_both_na: different values return FALSE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_false(identical_or_both_na(TRUE, FALSE))
  expect_false(identical_or_both_na("a", "b"))
  expect_false(identical_or_both_na(1, 2))
})

test_that("identical_or_both_na: NA vs non-NA returns FALSE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_false(identical_or_both_na(NA, TRUE))
  expect_false(identical_or_both_na(TRUE, NA))
  expect_false(identical_or_both_na(NA, "hello"))
})

test_that("identical_or_both_na: NULL vs non-NULL returns FALSE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_false(identical_or_both_na(NULL, TRUE))
  expect_false(identical_or_both_na("a", NULL))
})

test_that("identical_or_both_na: NULL vs NA returns FALSE", {
  box::use(artma / data_config / defaults[identical_or_both_na])
  expect_false(identical_or_both_na(NULL, NA))
  expect_false(identical_or_both_na(NA, NULL))
})

# ── build_default_config_entry ────────────────────────────────────────────────

test_that("build_default_config_entry: numeric column", {
  box::use(artma / data_config / defaults[build_default_config_entry])

  withr::local_options(list(
    "artma.data.na_handling" = "remove"
  ))

  col_data <- c(1.5, 2.3, 3.7)
  entry <- build_default_config_entry("my_var", col_data)

  expect_equal(entry$var_name, "my_var")
  expect_equal(entry$var_name_verbose, entry$var_name_description)
  expect_true(entry$variable_summary)
  expect_equal(entry$na_handling, "remove")
  expect_true(is.na(entry$bma))
  expect_true(is.na(entry$effect_sum_stats))
  expect_true(is.na(entry$equal))
  expect_true(is.na(entry$gltl))
  expect_true(is.na(entry$group_category))
})

test_that("build_default_config_entry: character column sets variable_summary FALSE", {
  box::use(artma / data_config / defaults[build_default_config_entry])

  withr::local_options(list(
    "artma.data.na_handling" = "remove"
  ))

  col_data <- c("a", "b", "c")
  entry <- build_default_config_entry("category", col_data)

  expect_equal(entry$var_name, "category")
  expect_false(entry$variable_summary)
})

test_that("build_default_config_entry: entry has all 17 expected fields", {
  box::use(artma / data_config / defaults[build_default_config_entry])

  withr::local_options(list(
    "artma.data.na_handling" = "remove"
  ))

  entry <- build_default_config_entry("x", c(1, 2, 3))
  expected_fields <- c(
    "var_name", "var_name_verbose", "var_name_description",
    "data_type", "group_category", "na_handling", "variable_summary",
    "effect_sum_stats", "equal", "gltl", "bma", "bma_reference_var",
    "bma_to_log", "bpe", "bpe_sum_stats", "bpe_equal", "bpe_gltl"
  )
  expect_equal(sort(names(entry)), sort(expected_fields))
})

# ── build_base_config ─────────────────────────────────────────────────────────

test_that("build_base_config: creates entry for each column", {
  box::use(artma / data_config / defaults[build_base_config])

  withr::local_options(list(
    "artma.data.na_handling" = "remove"
  ))

  df <- data.frame(
    age = c(25, 30, 35),
    country = c("US", "UK", "DE"),
    stringsAsFactors = FALSE
  )

  config <- build_base_config(df)

  expect_length(config, 2)
  expect_true("age" %in% names(config))
  expect_true("country" %in% names(config))
  expect_equal(config$age$var_name, "age")
  expect_equal(config$country$var_name, "country")
  expect_true(config$age$variable_summary)
  expect_false(config$country$variable_summary)
})

test_that("build_base_config: keys are make.names of column names", {
  box::use(artma / data_config / defaults[build_base_config])

  withr::local_options(list(
    "artma.data.na_handling" = "remove"
  ))

  df <- data.frame(
    a = 1,
    b = 2,
    check.names = FALSE
  )
  names(df) <- c("my var", "x.y")

  config <- build_base_config(df)

  expect_true("my.var" %in% names(config))
  expect_true("x.y" %in% names(config))
  expect_equal(config[["my.var"]]$var_name, "my var")
})

test_that("build_base_config: errors on empty dataframe", {
  box::use(artma / data_config / defaults[build_base_config])

  df <- data.frame(x = numeric(0))
  expect_error(build_base_config(df), "empty")
})

# ── extract_overrides ─────────────────────────────────────────────────────────

test_that("extract_overrides: returns NULL when entry matches defaults", {
  box::use(artma / data_config / defaults[extract_overrides])

  entry <- list(var_name = "x", bma = NA, gltl = NA, equal = NA)
  default <- list(var_name = "x", bma = NA, gltl = NA, equal = NA)

  result <- extract_overrides(entry, default)
  expect_null(result)
})

test_that("extract_overrides: returns only changed fields", {
  box::use(artma / data_config / defaults[extract_overrides])

  default <- list(var_name = "x", bma = NA, gltl = NA, equal = NA)
  entry <- list(var_name = "x", bma = TRUE, gltl = NA, equal = "median")

  result <- extract_overrides(entry, default)

  expect_equal(result$bma, TRUE)
  expect_equal(result$equal, "median")
  expect_null(result$gltl)
  expect_null(result$var_name)
})

test_that("extract_overrides: skips var_name even if different", {
  box::use(artma / data_config / defaults[extract_overrides])

  default <- list(var_name = "x", bma = NA)
  entry <- list(var_name = "modified_x", bma = NA)

  result <- extract_overrides(entry, default)
  expect_null(result)
})

test_that("extract_overrides: detects change from NA to value", {
  box::use(artma / data_config / defaults[extract_overrides])

  default <- list(var_name = "x", effect_sum_stats = NA)
  entry <- list(var_name = "x", effect_sum_stats = TRUE)

  result <- extract_overrides(entry, default)
  expect_equal(result$effect_sum_stats, TRUE)
})

test_that("extract_overrides: detects change from value to different value", {
  box::use(artma / data_config / defaults[extract_overrides])

  default <- list(var_name = "x", na_handling = "remove")
  entry <- list(var_name = "x", na_handling = "impute")

  result <- extract_overrides(entry, default)
  expect_equal(result$na_handling, "impute")
})
