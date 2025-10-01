box::use(
  testthat[test_that, expect_equal, test_path, skip_on_cran]
)
box::use(artma / calc / methods / stem[stem, stem_converge, stem_compute, variance_b, variance_0, weighted_mean, weighted_mean_squared]) # nolint

skip_on_cran()

legacy_stem_env <- function() {
  env <- new.env()
  env$load_packages <- function(...) NULL
  sys.source(test_path("..", "..", "utils", "src", "methods", "stem_method_master_thesis_cala.R"), envir = env)
  env
}

create_stem_fixture <- function() {
  beta <- c(0.2, 0.15, 0.1, 0.05, 0)
  se <- c(0.1, 0.12, 0.11, 0.13, 0.14)
  list(beta = beta, se = se, param = c(1e-4, 100))
}

test_that("core stem computation matches legacy implementation", {
  env <- legacy_stem_env()
  fixture <- create_stem_fixture()
  legacy <- env$stem(fixture$beta, fixture$se, fixture$param)
  modern <- stem(fixture$beta, fixture$se, fixture$param)
  expect_equal(modern$estimates, legacy$estimates)
  expect_equal(modern$MSE, legacy$MSE)
})

test_that("helper functions align with legacy counterparts", {
  env <- legacy_stem_env()
  fixture <- create_stem_fixture()
  sorted <- cbind(fixture$beta, fixture$se)[order(fixture$se), ]
  beta_sorted <- sorted[, 1]
  se_sorted <- sorted[, 2]
  expect_equal(weighted_mean(beta_sorted, se_sorted, 0.1), env$weighted_mean(beta_sorted, se_sorted, 0.1))
  expect_equal(weighted_mean_squared(beta_sorted, se_sorted, 0.1), env$weighted_mean_squared(beta_sorted, se_sorted, 0.1))
  expect_equal(variance_b(se_sorted, 0.1), env$variance_b(se_sorted, 0.1))
  expect_equal(variance_0(length(beta_sorted), beta_sorted, se_sorted, mean(beta_sorted)), env$variance_0(length(beta_sorted), beta_sorted, se_sorted, mean(beta_sorted)))
  modern_compute <- stem_compute(beta_sorted, se_sorted, 0.1)
  legacy_compute <- env$stem_compute(beta_sorted, se_sorted, 0.1)
  expect_equal(modern_compute$estimates, legacy_compute$estimates)
  expect_equal(modern_compute$MSE, legacy_compute$MSE)
  modern_converge <- stem_converge(0.1, beta_sorted, se_sorted, fixture$param)
  legacy_converge <- env$stem_converge(0.1, beta_sorted, se_sorted, fixture$param)
  expect_equal(modern_converge$estimates, legacy_converge$estimates)
  expect_equal(modern_converge$MSE, legacy_converge$MSE)
})
