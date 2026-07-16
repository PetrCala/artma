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

# ── read_df_for_config caching ────────────────────────────────────────────────

test_that("read_df_for_config uses a primed dataframe cache without re-reading data", {
  box::use(
    artma / data / read[read_data],
    artma / data_config / resolve[
      prime_df_for_config_cache,
      read_df_for_config
    ],
    testing / mocks / index[MOCKS]
  )

  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)
  withr::defer(unlink(tmp_file))

  withr::local_options(list(
    "artma.data.source_path" = tmp_file,
    "artma.verbose" = 3
  ))

  loaded_df <- read_data(tmp_file)
  prime_df_for_config_cache(loaded_df, tmp_file)

  replay_msgs <- testthat::capture_messages(read_df_for_config())
  read_msgs <- grep("Reading data from", replay_msgs, value = TRUE)

  expect_equal(length(read_msgs), 0L)
  expect_equal(nrow(read_df_for_config()), nrow(loaded_df))
})

test_that("read_df_for_config re-reads when the source file changes at the same path", {
  box::use(
    artma / data_config / resolve[read_df_for_config],
    testing / mocks / index[MOCKS]
  )

  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)
  withr::defer(unlink(tmp_file))

  withr::local_options(list(
    "artma.data.source_path" = tmp_file,
    "artma.verbose" = 1
  ))

  first <- read_df_for_config()
  expect_false("new_moderator" %in% colnames(first))

  # Rewrite the file at the same path with an extra column and bump the
  # modification time so the change is unambiguous even on coarse filesystems
  df$new_moderator <- seq_len(nrow(df))
  utils::write.csv(df, tmp_file, row.names = FALSE)
  Sys.setFileTime(tmp_file, Sys.time() + 5)

  second <- read_df_for_config()
  expect_true("new_moderator" %in% colnames(second))
})

test_that("get_data_config picks up file changes at the same source path", {
  box::use(
    artma / data_config / read[get_data_config],
    testing / mocks / index[MOCKS]
  )

  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)
  withr::defer(unlink(tmp_file))

  withr::local_options(list(
    "artma.data.source_path" = tmp_file,
    "artma.data.columns" = list(),
    "artma.verbose" = 1
  ))

  config_before <- get_data_config()
  expect_false("new_moderator" %in% names(config_before))

  df$new_moderator <- seq_len(nrow(df))
  utils::write.csv(df, tmp_file, row.names = FALSE)
  Sys.setFileTime(tmp_file, Sys.time() + 5)

  config_after <- get_data_config()
  expect_true("new_moderator" %in% names(config_after))
})

# ── canonical config keys ─────────────────────────────────────────────────────

test_that("config keys are standardized regardless of which entry point resolves first", {
  box::use(
    artma / data / read[read_data],
    artma / data / utils[standardize_column_names],
    artma / data_config / read[get_data_config],
    artma / data_config / resolve[prime_df_for_config_cache],
    testing / mocks / index[MOCKS]
  )

  raw_map <- list(
    study_id = "study",
    effect = "coef",
    se = "stderr",
    n_obs = "nobs"
  )
  df <- MOCKS$create_mock_df(seed = 42, colnames_map = raw_map)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)
  withr::defer(unlink(tmp_file))

  withr::local_options(list(
    "artma.data.source_path" = tmp_file,
    "artma.data.columns" = list(
      study_id = list(source_name = "study"),
      effect = list(source_name = "coef"),
      se = list(source_name = "stderr"),
      n_obs = list(source_name = "nobs")
    ),
    "artma.verbose" = 1
  ))

  # Cold cache: this is what public entry points (config.get) hit first
  cold_config <- get_data_config()

  expect_true(all(c("study_id", "effect", "se", "n_obs") %in% names(cold_config)))
  expect_false(any(c("coef", "stderr", "nobs") %in% names(cold_config)))

  # Pipeline path: prepare_data standardizes the df and primes the cache
  df_std <- standardize_column_names(read_data(tmp_file), auto_detect = FALSE)
  prime_df_for_config_cache(df_std, tmp_file)
  primed_config <- get_data_config()

  expect_equal(sort(names(cold_config)), sort(names(primed_config)))
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
    "artma.data.columns" = config,
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
    "artma.data.columns" = list(),
    "artma.data.source_path" = NA,
    "artma.verbose" = 1
  ))

  result <- get_data_config(create_if_missing = TRUE)
  expect_equal(result, list())
})

test_that("get_data_config: merges overrides onto base when df is available", {
  box::use(
    artma / data_config / read[get_data_config],
    testing / mocks / index[MOCKS]
  )

  # Create a real temp CSV file
  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)
  withr::defer(unlink(tmp_file))

  withr::local_options(list(
    "artma.data.columns" = list(
      effect = list(bma = TRUE)
    ),
    "artma.data.source_path" = tmp_file,
    "artma.verbose" = 1,
    "artma.data.na_handling" = "remove"
  ))

  result <- get_data_config()

  # Should have all columns from the df
  expect_true("effect" %in% names(result))
  expect_true("se" %in% names(result))

  # The override should be applied
  expect_equal(result$effect$bma, TRUE)

  # Non-overridden fields should have defaults
  expect_true(is.na(result$se$bma))
})
