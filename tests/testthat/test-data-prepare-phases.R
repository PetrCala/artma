box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_true, test_that],
  testing / fixtures / index[FIXTURES],
  testing / mocks / index[MOCKS]
)

# Both tests drive the same three-phase pipeline over equivalent inputs, so the
# expensive fixture work (mock CSV plus options.create/options.load, most of
# this file's runtime) happens once at file scope. options.load() is pure: it
# returns the prefixed list (including artma.temp.* bookkeeping) without
# touching options(), so each test activates the shared list with
# withr::local_options() and stays isolated; every artma.* key the pipeline
# writes during a test is present in the list and therefore restored on exit.
# The dataset is deliberately tiny: these tests exercise cache and config
# bookkeeping, not data richness.
shared_runtime_options <- local({
  fixture_dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())

  df <- MOCKS$create_mock_df(seed = 42, nrow = 60, n_studies = 5)
  source_path <- file.path(fixture_dir, "phases-data.csv")
  utils::write.csv(df, source_path, row.names = FALSE)

  options_dir <- file.path(fixture_dir, "options")
  dir.create(options_dir)

  artma::options.create(
    options_file_name = "phases.yaml",
    options_dir = options_dir,
    user_input = list(
      "data.source_path" = source_path,
      "data.na_handling" = "remove",
      "data.reconcile_mode" = "auto",
      "calc.se_zero_handling" = "ignore",
      "cache.use_cache" = TRUE,
      "verbose" = 1L
    ),
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
})

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
  self_env <- environment()
  counted_compute <- function() {
    self_env$compute_runs <- self_env$compute_runs + 1L
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

  list(run = run, compute_runs = function() self_env$compute_runs)
}

test_that("warm-cache run repopulates a deleted data config (motivating bug)", {
  FIXTURES$local_cli_silence()

  withr::local_options(shared_runtime_options)

  runner <- local_phase_runner()

  # Run 1 (cold): computes and registers the computed columns in the config.
  runner$run()
  expect_equal(runner$compute_runs(), 1L)

  after_first <- getOption("artma.data.columns")
  computed_keys <- c("t_stat", "study_size", "reg_dof", "precision", "study_label")
  expect_true(all(computed_keys %in% names(after_first)))

  # Simulate deleting the data config: clear it entirely.
  options("artma.data.columns" = list())

  # Run 2 (warm): compute is a cache hit and does NOT run, but persist must
  # still repopulate the config.
  runner$run()
  expect_equal(runner$compute_runs(), 1L) # compute was skipped (cache hit)

  after_second <- getOption("artma.data.columns")
  expect_true(all(computed_keys %in% names(after_second)))
})

test_that("non-interactive prepare is identical run to run with cache hits from run 2", {
  FIXTURES$local_cli_silence()

  withr::local_options(shared_runtime_options)

  runner <- local_phase_runner()

  first <- runner$run()
  expect_equal(runner$compute_runs(), 1L)

  second <- runner$run()
  expect_equal(runner$compute_runs(), 1L) # run 2 hits the cache
  expect_identical(first, second)
})
