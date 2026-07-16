box::use(
  testthat[test_that, expect_equal, expect_false, expect_identical],
  artma / testing / fixtures / index[FIXTURES],
  artma / testing / mocks / index[MOCKS]
)

# Computed-column config entries as the pipeline writes them. Seeding these
# keeps prepare_data() from persisting data-config updates to a runtime
# options file, which unit tests do not have.
precomputed_config_overrides <- function() {
  list(
    obs_id = list(var_name = "obs_id", is_computed = TRUE),
    study_id = list(var_name = "study_id", is_computed = TRUE),
    study_label = list(var_name = "study_label", is_computed = TRUE),
    t_stat = list(var_name = "t_stat", is_computed = TRUE),
    study_size = list(var_name = "study_size", is_computed = TRUE),
    reg_dof = list(var_name = "reg_dof", is_computed = TRUE),
    precision = list(var_name = "precision", is_computed = TRUE)
  )
}

pipeline_options <- function(tmp_file) {
  list(
    "artma.cache.use_cache" = TRUE,
    "artma.data.source_path" = tmp_file,
    "artma.data.colnames.study_id" = "study_id",
    "artma.data.colnames.effect" = "effect",
    "artma.data.colnames.se" = "se",
    "artma.data.colnames.n_obs" = "n_obs",
    "artma.data.config" = precomputed_config_overrides(),
    "artma.data.config_setup" = "auto",
    "artma.data.expected_schema_columns" = NULL,
    "artma.data.na_handling" = "remove",
    "artma.data.reconcile_mode" = "auto",
    "artma.calc.se_zero_handling" = "ignore",
    "artma.temp.file_name" = NULL,
    "artma.temp.dir_name" = NULL,
    "artma.verbose" = 1
  )
}

test_that("build_data_cache_signature ignores options written by the pipeline", {
  box::use(artma / data / cache_signatures[build_data_cache_signature])

  tmp_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1), tmp_file, row.names = FALSE)

  withr::local_options(list(
    "artma.data.source_path" = tmp_file,
    "artma.data.config" = list(
      gdp = list(var_name = "gdp", effect_sum_stats = TRUE)
    ),
    "artma.data.expected_schema_columns" = NULL,
    "artma.data.na_handling" = "remove",
    "artma.temp.file_name" = NULL,
    "artma.temp.dir_name" = NULL
  ))

  before <- build_data_cache_signature()

  # Simulate what prepare_data() writes during a run: computed column entries
  # in the data config, the schema baseline, and session bookkeeping.
  mutated_config <- getOption("artma.data.config")
  mutated_config$precision <- list(var_name = "precision", is_computed = TRUE)
  mutated_config$obs_id <- list(var_name = "obs_id", is_computed = TRUE)

  withr::local_options(list(
    "artma.data.config" = mutated_config,
    "artma.data.expected_schema_columns" = c("gdp", "effect", "se"),
    "artma.temp.file_name" = "unit-test.yaml",
    "artma.temp.dir_name" = withr::local_tempdir()
  ))

  after <- build_data_cache_signature()

  expect_identical(before, after)
})

test_that("build_data_cache_signature reacts to user-authored inputs", {
  box::use(artma / data / cache_signatures[build_data_cache_signature])

  tmp_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1), tmp_file, row.names = FALSE)

  withr::local_options(list(
    "artma.data.source_path" = tmp_file,
    "artma.data.config" = list(
      gdp = list(var_name = "gdp", effect_sum_stats = TRUE)
    ),
    "artma.data.na_handling" = "remove"
  ))

  baseline <- build_data_cache_signature()

  # A preprocessing option changes the signature
  changed_option <- withr::with_options(
    list("artma.data.na_handling" = "median"),
    build_data_cache_signature()
  )
  expect_false(identical(baseline, changed_option))

  # A user-authored data config entry changes the signature
  changed_config <- withr::with_options(
    list("artma.data.config" = list(
      gdp = list(var_name = "gdp", effect_sum_stats = FALSE)
    )),
    build_data_cache_signature()
  )
  expect_false(identical(baseline, changed_config))
})

test_that("compute phase hits the cache on a second identical run", {
  box::use(
    artma / data / index[compute_data_impl, prime_raw_df],
    artma / data / read[read_data],
    artma / data / cache_signatures[build_data_cache_signature],
    artma / libs / infrastructure / cache[cache_cli_runner]
  )

  FIXTURES$local_cli_silence()

  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)

  withr::local_options(pipeline_options(tmp_file))

  # The raw frame is read once and handed to the cached compute phase, exactly
  # as prepare_data() does.
  prime_raw_df(read_data(tmp_file))

  # The config df cache keys on path + mtime, so entries left behind by other
  # tests cannot collide with this test's temp file.
  runs <- 0L
  counted_impl <- function() {
    runs <<- runs + 1L
    compute_data_impl()
  }

  # Same wiring as compute_data in artma/data/index.R, but with an isolated
  # in-memory cache so the test does not touch the user cache directory.
  runner <- cache_cli_runner(
    counted_impl,
    stage = "prepare_data",
    key_builder = function(...) build_data_cache_signature(),
    cache = memoise::cache_memory()
  )

  # Run 1 computes.
  first <- testthat::capture_messages(df_first <- runner())
  expect_equal(runs, 1L)

  # Run 2 with identical inputs must be a cache hit, not a recomputation.
  second <- testthat::capture_messages(df_second <- runner())
  expect_equal(runs, 1L)
  expect_identical(df_first, df_second)

  # Changing a user-authored preprocessing option must cause a cache miss.
  withr::local_options(list("artma.data.na_handling" = "median"))
  testthat::capture_messages(runner())
  expect_equal(runs, 2L)
})

test_that("compute phase is pure: it does not mutate options()", {
  box::use(
    artma / data / index[compute_data_impl, prime_raw_df],
    artma / data / read[read_data]
  )

  FIXTURES$local_cli_silence()

  df <- MOCKS$create_mock_df(seed = 7)
  tmp_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)

  # Cache off so the implementation actually runs and could mutate options.
  withr::local_options(utils::modifyList(
    pipeline_options(tmp_file),
    list("artma.cache.use_cache" = FALSE)
  ))

  prime_raw_df(read_data(tmp_file))

  artma_options <- function() {
    opts <- options()
    opts[grepl("^artma\\.", names(opts))]
  }

  before <- artma_options()
  testthat::capture_messages(compute_data_impl())
  after <- artma_options()

  expect_identical(after, before)
})
