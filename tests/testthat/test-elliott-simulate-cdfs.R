box::use(
  testthat[
    expect_equal,
    expect_length,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_options],
  artma / calc / methods / elliott[simulate_cdfs_parallel]
)

run_simulation <- function(seed, iterations, grid_points, show_progress = FALSE, verbose = 0) {
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  local_options(list(
    artma.verbose = verbose,
    artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = FALSE
  ))
  set.seed(seed)
  simulate_cdfs_parallel(
    iterations = iterations,
    grid_points = grid_points,
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
