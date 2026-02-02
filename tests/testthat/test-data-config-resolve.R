box::use(
  testthat[
    expect_equal,
    expect_null,
    expect_true,
    expect_length,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_null <- getFromNamespace("expect_null", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_length <- getFromNamespace("expect_length", "testthat")

# ── merge_config ──────────────────────────────────────────────────────────────

test_that("merge_config: returns base when overrides are empty", {
  box::use(artma / data_config / resolve[merge_config])

  base <- list(
    x = list(var_name = "x", bma = NA, gltl = NA),
    y = list(var_name = "y", bma = NA, gltl = NA)
  )

  result <- merge_config(base, list())
  expect_equal(result, base)
})

test_that("merge_config: returns base when overrides are NULL", {
  box::use(artma / data_config / resolve[merge_config])

  base <- list(x = list(bma = NA))
  result <- merge_config(base, NULL)
  expect_equal(result, base)
})

test_that("merge_config: overlays specific fields on existing variable", {
  box::use(artma / data_config / resolve[merge_config])

  base <- list(
    my_var = list(
      var_name = "my_var",
      bma = NA,
      gltl = NA,
      equal = NA,
      variable_summary = TRUE
    )
  )

  overrides <- list(
    my_var = list(
      bma = TRUE,
      gltl = "median"
    )
  )

  result <- merge_config(base, overrides)

  expect_equal(result$my_var$bma, TRUE)
  expect_equal(result$my_var$gltl, "median")
  expect_true(is.na(result$my_var$equal))
  expect_equal(result$my_var$var_name, "my_var")
  expect_equal(result$my_var$variable_summary, TRUE)
})

test_that("merge_config: adds new variable not in base", {
  box::use(artma / data_config / resolve[merge_config])

  base <- list(
    x = list(var_name = "x", bma = NA)
  )

  overrides <- list(
    new_col = list(bma = TRUE, effect_sum_stats = TRUE)
  )

  result <- merge_config(base, overrides)

  expect_length(result, 2)
  expect_true("new_col" %in% names(result))
  expect_equal(result$new_col$bma, TRUE)
  expect_equal(result$new_col$effect_sum_stats, TRUE)
  expect_equal(result$x$var_name, "x")
})

test_that("merge_config: overrides multiple variables independently", {
  box::use(artma / data_config / resolve[merge_config])

  base <- list(
    a = list(var_name = "a", bma = NA, gltl = NA),
    b = list(var_name = "b", bma = NA, gltl = NA)
  )

  overrides <- list(
    a = list(bma = TRUE),
    b = list(gltl = "median")
  )

  result <- merge_config(base, overrides)

  expect_equal(result$a$bma, TRUE)
  expect_true(is.na(result$a$gltl))
  expect_true(is.na(result$b$bma))
  expect_equal(result$b$gltl, "median")
})

test_that("merge_config: skips non-list override entries", {
  box::use(artma / data_config / resolve[merge_config])

  base <- list(
    x = list(var_name = "x", bma = NA)
  )

  overrides <- list(
    x = "not a list"
  )

  result <- merge_config(base, overrides)

  # Non-list override is skipped, base is unchanged
  expect_equal(result$x$var_name, "x")
  expect_true(is.na(result$x$bma))
})

# ── invalidate_df_cache ───────────────────────────────────────────────────────

test_that("invalidate_df_cache: clears internal cache state", {
  box::use(artma / data_config / resolve[invalidate_df_cache])

  # Just verify it runs without error
  invalidate_df_cache()
})

# ── get_data_config (integration with resolve) ───────────────────────────────

test_that("get_data_config: returns overrides when df not available", {
  box::use(artma / data_config / read[get_data_config])

  config <- list(
    my_var = list(
      var_name = "my_var",
      bma = TRUE,
      gltl = "median"
    )
  )

  withr::local_options(list(
    "artma.data.config" = config,
    "artma.data.source_path" = NA,
    "artma.verbose" = 1
  ))

  result <- get_data_config()
  expect_equal(result$my_var$bma, TRUE)
  expect_equal(result$my_var$gltl, "median")
})

test_that("get_data_config: returns empty list when no config and no df", {
  box::use(artma / data_config / read[get_data_config])

  withr::local_options(list(
    "artma.data.config" = list(),
    "artma.data.source_path" = NA,
    "artma.verbose" = 1
  ))

  result <- get_data_config(create_if_missing = TRUE)
  expect_equal(result, list())
})

test_that("get_data_config: merges overrides onto base when df is available", {
  box::use(
    artma / data_config / read[get_data_config],
    artma / data_config / resolve[invalidate_df_cache],
    artma / testing / mocks / index[MOCKS]
  )

  # Create a real temp CSV file
  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)

  withr::local_options(list(
    "artma.data.config" = list(
      effect = list(bma = TRUE)
    ),
    "artma.data.source_path" = tmp_file,
    "artma.verbose" = 1,
    "artma.data.na_handling" = "remove"
  ))
  invalidate_df_cache()

  result <- get_data_config()

  # Should have all columns from the df
  expect_true("effect" %in% names(result))
  expect_true("se" %in% names(result))

  # The override should be applied
  expect_equal(result$effect$bma, TRUE)

  # Non-overridden fields should have defaults
  expect_true(is.na(result$se$bma))

  unlink(tmp_file)
  invalidate_df_cache()
})
