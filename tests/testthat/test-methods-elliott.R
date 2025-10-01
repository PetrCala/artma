box::use(testthat[test_that, expect_equal, test_path, skip_on_cran])

skip_on_cran()

legacy_elliott_env <- function() {
  env <- new.env()
  env$linspace <- function(start_, stop_, n) seq(from = start_, to = stop_, length.out = n)
  sys.source(test_path("..", "..", "utils", "src", "methods", "elliott_master_thesis_cala.R"), envir = env)
  env
}

fixture_pvalues <- function() {
  c(0.01, 0.02, 0.03, 0.04, 0.05)
}

test_that("binomial test matches legacy implementation", {
  box::use(artma / calc / methods / elliott[binomial_test]) # nolint
  env <- legacy_elliott_env()
  P <- fixture_pvalues()
  expect_equal(binomial_test(P, 0, 0.05, "c"), env$Binomial(P, 0, 0.05, "c"))
  expect_equal(binomial_test(P, 0, 0.05, "o"), env$Binomial(P, 0, 0.05, "o"))
})

test_that("fisher test aligns with legacy implementation", {
  box::use(artma / calc / methods / elliott[fisher_test]) # nolint
  env <- legacy_elliott_env()
  P <- fixture_pvalues()
  expect_equal(fisher_test(P, 0, 0.05), env$Fisher(P, 0, 0.05))
})

test_that("lambda2 and bounds reproduce legacy values", {
  box::use(artma / calc / methods / elliott[lambda2, bound0, bound1, bound2]) # nolint
  env <- legacy_elliott_env()
  h <- seq(0, 0.5, by = 0.1)
  expect_equal(lambda2(0.01, 0.02, h), env$lambda2(0.01, 0.02, h))
  expect_equal(bound0(0.05, 5), env$Bound0(0.05, 5))
  expect_equal(bound1(0.05, 5), env$Bound1(0.05, 5))
  expect_equal(bound2(0.05, 6), env$Bound2(0.05, 6))
})
