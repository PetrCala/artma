box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_true, test_that],
  testing / fixtures / index[FIXTURES],
  testing / mocks / index[MOCKS]
)

# Create a valid runtime options file pointing at `source_path`, load it, and
# return the prefixed options list (including artma.temp.* bookkeeping) so a
# test can activate it with withr::local_options().
local_runtime_options <- function(source_path, extra = list()) {
  options_dir <- withr::local_tempdir(.local_envir = parent.frame())

  user_input <- utils::modifyList(
    list(
      "data.source_path" = source_path,
      "data.colnames.study_id" = "study_id",
      "data.colnames.effect" = "effect",
      "data.colnames.se" = "se",
      "data.colnames.n_obs" = "n_obs",
      "data.na_handling" = "remove",
      "data.reconcile_mode" = "auto",
      "calc.se_zero_handling" = "ignore",
      "cache.use_cache" = TRUE,
      "verbose" = 1L
    ),
    extra
  )

  artma::options.create(
    options_file_name = "phases.yaml",
    options_dir = options_dir,
    user_input = user_input,
    should_validate = TRUE,
    should_overwrite = TRUE
  )

  artma::options.load(
    options_file_name = "phases.yaml",
    options_dir = options_dir,
    load_with_prefix = TRUE,
    should_validate = TRUE,
    should_add_temp_options = TRUE,
    should_return = TRUE
  )
}

# Orchestrate the three phases with an isolated in-memory compute cache, so the
# test never touches the user cache directory. Mirrors prepare_data() exactly.
local_phase_runner <- function() {
  box::use(
    artma / data / index[configure_data, compute_data_impl, persist_data, prime_raw_df],
    artma / data / read[read_data],
    artma / data / cache_signatures[build_data_cache_signature],
    artma / libs / infrastructure / cache[cache_cli_runner]
  )

  compute_runs <- 0L
  counted_compute <- function() {
    compute_runs <<- compute_runs + 1L
    compute_data_impl()
  }
  compute_cached <- cache_cli_runner(
    counted_compute,
    stage = "prepare_data",
    key_builder = function(...) build_data_cache_signature(),
    cache = memoise::cache_memory()
  )

  run <- function() {
    df_raw <- read_data()
    prime_raw_df(df_raw)
    configure_data(df_raw)
    df <- compute_cached()
    persist_data(df)
    df
  }

  list(run = run, compute_runs = function() compute_runs)
}

test_that("warm-cache run repopulates a deleted data config (motivating bug)", {
  FIXTURES$local_cli_silence()

  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)

  withr::local_options(local_runtime_options(tmp_file))

  runner <- local_phase_runner()

  # Run 1 (cold): computes and registers the computed columns in the config.
  runner$run()
  expect_equal(runner$compute_runs(), 1L)

  after_first <- getOption("artma.data.config")
  computed_keys <- c("t_stat", "study_size", "reg_dof", "precision", "study_label")
  expect_true(all(computed_keys %in% names(after_first)))

  # Simulate deleting the data config: clear it entirely.
  options("artma.data.config" = list())

  # Run 2 (warm): compute is a cache hit and does NOT run, but persist must
  # still repopulate the config.
  runner$run()
  expect_equal(runner$compute_runs(), 1L) # compute was skipped (cache hit)

  after_second <- getOption("artma.data.config")
  expect_true(all(computed_keys %in% names(after_second)))
})

test_that("non-interactive prepare is identical run to run with cache hits from run 2", {
  FIXTURES$local_cli_silence()

  df <- MOCKS$create_mock_df(seed = 7)
  tmp_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)

  withr::local_options(local_runtime_options(tmp_file))

  runner <- local_phase_runner()

  first <- runner$run()
  expect_equal(runner$compute_runs(), 1L)

  second <- runner$run()
  expect_equal(runner$compute_runs(), 1L) # run 2 hits the cache
  expect_identical(first, second)
})
