box::use(
  testthat[
    expect_equal,
    expect_identical,
    expect_length,
    expect_true,
    skip_if_not,
    test_that
  ],
  withr[local_options, local_tempdir],
  artma / calc / methods / elliott[simulate_cdfs_block_cpp, simulate_cdfs_parallel],
  artma / calc / methods / elliott_cache[make_simulate_cdfs_cached]
)

# Deterministic, quiet defaults for every test: the R fallback keeps results
# identical across platforms, and caching stays on unless a test disables it.
local_sim_options <- function(env = parent.frame()) {
  local_options(
    list(
      artma.verbose = 1,
      artma.cache.use_cache = TRUE,
      artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = FALSE
    ),
    .local_envir = env
  )
}

# A cached instance backed by an ephemeral disk cache, with a wrapper that
# counts how many times the real simulation runs.
new_cached_instance <- function(env = parent.frame()) {
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  counting_sim <- function(...) {
    counter$calls <- counter$calls + 1L
    simulate_cdfs_parallel(...)
  }
  cache <- memoise::cache_filesystem(local_tempdir(.local_envir = env))
  list(
    simulate = make_simulate_cdfs_cached(cache = cache, sim_fun = counting_sim),
    calls = function() counter$calls,
    cache = cache
  )
}

test_that("a warm cache returns the identical vector without re-simulating", {
  local_sim_options()
  inst <- new_cached_instance()

  cold <- inst$simulate(iterations = 4, grid_points = 24, show_progress = FALSE, seed = 123)
  expect_equal(inst$calls(), 1L)
  expect_length(inst$cache$keys(), 1L)

  warm <- inst$simulate(iterations = 4, grid_points = 24, show_progress = FALSE, seed = 123)
  expect_equal(inst$calls(), 1L)
  expect_identical(warm, cold)
  expect_length(inst$cache$keys(), 1L)
})

test_that("a cache hit leaves the session RNG where a cold run would", {
  local_sim_options()
  inst <- new_cached_instance()

  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 42)
  after_cold <- stats::rnorm(2)

  # Scramble the session RNG so only the restored state can line the draws up.
  set.seed(999)
  stats::runif(5)

  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 42)
  expect_equal(inst$calls(), 1L)
  after_warm <- stats::rnorm(2)

  expect_identical(after_warm, after_cold)
})

test_that("seed = NULL bypasses the cache and consumes the caller's RNG state", {
  local_sim_options()
  inst <- new_cached_instance()

  set.seed(7)
  first <- inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE)
  second <- inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE)

  expect_equal(inst$calls(), 2L)
  expect_length(inst$cache$keys(), 0L)
  expect_true(any(first != second))
})

test_that("disabling artma.cache.use_cache bypasses the cache at call time", {
  local_sim_options()
  inst <- new_cached_instance()

  local_options(list(artma.cache.use_cache = FALSE))
  first <- inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 123)
  second <- inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 123)

  expect_equal(inst$calls(), 2L)
  expect_length(inst$cache$keys(), 0L)
  expect_identical(first, second)
})

test_that("the cache key separates seeds, iteration counts, and grid sizes", {
  local_sim_options()
  inst <- new_cached_instance()

  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 123)
  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 124)
  inst$simulate(iterations = 4, grid_points = 20, show_progress = FALSE, seed = 123)
  inst$simulate(iterations = 3, grid_points = 24, show_progress = FALSE, seed = 123)

  expect_equal(inst$calls(), 4L)
  expect_length(inst$cache$keys(), 4L)

  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 123)
  expect_equal(inst$calls(), 4L)
})

test_that("the cache key separates the compiled backend from the R fallback", {
  cpp_available <- tryCatch(
    {
      simulate_cdfs_block_cpp(matrix(0, nrow = 1, ncol = 1))
      TRUE
    },
    error = function(e) FALSE
  )
  skip_if_not(cpp_available, "compiled simulate_cdfs backend is unavailable")

  local_options(list(artma.verbose = 1, artma.cache.use_cache = TRUE))
  inst <- new_cached_instance()

  local_options(list(artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = TRUE))
  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 123)
  expect_equal(inst$calls(), 1L)

  local_options(list(artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = FALSE))
  inst$simulate(iterations = 3, grid_points = 20, show_progress = FALSE, seed = 123)
  expect_equal(inst$calls(), 2L)
  expect_length(inst$cache$keys(), 2L)
})
