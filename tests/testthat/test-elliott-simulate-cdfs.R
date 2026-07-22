box::use(
  testthat[
    expect_equal,
    expect_length,
    expect_null,
    expect_true,
    skip_if_not,
    test_that
  ],
  withr[local_options],
  artma / calc / methods / elliott[simulate_cdfs_parallel]
)

run_simulation <- function(seed, iterations, grid_points, show_progress = FALSE, verbose = 0,
                           use_cpp = FALSE, workers = NULL) {
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  local_options(list(
    artma.verbose = verbose,
    artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = use_cpp
  ))
  set.seed(seed)
  simulate_cdfs_parallel(
    iterations = iterations,
    grid_points = grid_points,
    workers = workers,
    show_progress = show_progress
  )
}

test_that("simulate_cdfs returns numeric vectors with expected shape", {
  res <- run_simulation(seed = 123, iterations = 5, grid_points = 40)

  expect_length(res, 5L)
  expect_true(is.numeric(res))
  expect_null(names(res))
  expect_true(all(is.finite(res)))
  expect_true(all(res >= 0))
})

test_that("simulate_cdfs is deterministic for a fixed seed", {
  # Same seed must reproduce the same draws, both for a single iteration and
  # across several.
  param_sets <- list(
    list(seed = 123, iterations = 5, grid_points = 40),
    list(seed = 42, iterations = 1, grid_points = 12)
  )

  for (p in param_sets) {
    res_first <- run_simulation(seed = p$seed, iterations = p$iterations, grid_points = p$grid_points)
    res_second <- run_simulation(seed = p$seed, iterations = p$iterations, grid_points = p$grid_points)

    expect_equal(res_first, res_second, tolerance = 1e-12, info = sprintf("iterations = %d", p$iterations))
  }
})

test_that("simulate_cdfs output is unchanged when progress is suppressed", {
  tolerance <- 1e-12

  res_no_progress <- run_simulation(
    seed = 999,
    iterations = 3,
    grid_points = 30,
    show_progress = FALSE,
    verbose = 0
  )

  res_progress <- run_simulation(
    seed = 999,
    iterations = 3,
    grid_points = 30,
    show_progress = TRUE,
    verbose = 0
  )

  expect_equal(res_progress, res_no_progress, tolerance = tolerance)
})

test_that("simulate_cdfs responds to different RNG seeds", {
  res_a <- run_simulation(seed = 101, iterations = 3, grid_points = 30)
  res_b <- run_simulation(seed = 202, iterations = 3, grid_points = 30)

  expect_true(any(abs(res_a - res_b) > 0))
})

test_that("pure R fallback matches the compiled implementation elementwise", {
  cpp_available <- tryCatch(
    {
      .Call("_artma_simulate_cdfs_block_cpp", PACKAGE = "artma", matrix(0, 1, 1))
      TRUE
    },
    error = function(e) FALSE
  )
  skip_if_not(cpp_available, "compiled simulate_cdfs_block_cpp is unavailable")

  # Grid sizes where some gcmlcm x-knots times grid_points land just below an
  # integer (e.g. 86.999...). The fallback used to index with those raw
  # products, so truncation wrote hull chords one grid slot too low and its
  # suprema drifted from the C++ results by up to ~0.9 on a few draws.
  configs <- list(
    list(grid_points = 100L, iterations = 300L),
    list(grid_points = 700L, iterations = 300L)
  )

  for (cf in configs) {
    res_cpp <- run_simulation(
      seed = 42, iterations = cf$iterations, grid_points = cf$grid_points,
      use_cpp = TRUE, workers = 1L
    )
    res_r <- run_simulation(
      seed = 42, iterations = cf$iterations, grid_points = cf$grid_points,
      use_cpp = FALSE, workers = 1L
    )

    expect_length(res_r, cf$iterations)
    max_diff <- max(abs(res_r - res_cpp))
    expect_true(
      max_diff <= 1e-9,
      info = sprintf("grid_points = %d, max abs diff = %.3g", cf$grid_points, max_diff)
    )
  }
})
