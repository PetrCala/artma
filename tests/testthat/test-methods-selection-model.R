skip_on_cran()

box::use(
  artma / calc / methods / selection_model[
    clustered_covariance_estimate,
    compute_information_matrix,
    compute_score_matrix,
    robust_variance,
    compute_tpowers,
    variation_variance_loglikelihood,
    metastudies_estimation,
    estimates_table
  ]
)

legacy_selection_env <- function() {
  env <- new.env()
  sys.source(test_path("..", "..", "utils", "src", "methods", "selection_model_master_thesis_cala.R"), envir = env)
  env
}

create_selection_fixture <- function() {
  set.seed(123)
  X <- rnorm(6, 0.2, 0.4)
  sigma <- runif(6, 0.1, 0.3)
  list(X = X, sigma = sigma, cutoffs = c(1.96, 2.58), symmetric = TRUE)
}

test_that("indicator matrix matches legacy implementation", {
  env <- legacy_selection_env()
  fixture <- create_selection_fixture()
  modern <- compute_tpowers(fixture$X / fixture$sigma, fixture$cutoffs, fixture$symmetric)
  legacy <- env$Tpowers_fun(fixture$X / fixture$sigma, fixture$cutoffs, fixture$symmetric)
  expect_equal(modern, legacy)
})

test_that("log-likelihood aligns with legacy implementation", {
  env <- legacy_selection_env()
  fixture <- create_selection_fixture()
  tpowers <- compute_tpowers(fixture$X / fixture$sigma, fixture$cutoffs, fixture$symmetric)
  modern <- variation_variance_loglikelihood(0.1, 0.2, c(0.5, 0.5, 1), fixture$cutoffs, fixture$symmetric, fixture$X, fixture$sigma, tpowers)
  legacy <- env$VariationVarianceLogLikelihood(0.1, 0.2, c(0.5, 0.5, 1), fixture$cutoffs, fixture$symmetric, fixture$X, fixture$sigma, tpowers)
  expect_equal(modern$LLH, legacy$LLH)
  expect_equal(modern$logL, legacy$logL)
})

test_that("robust variance equals legacy implementation", {
  env <- legacy_selection_env()
  fixture <- create_selection_fixture()
  stepsize <- 1e-6
  tpowers <- compute_tpowers(fixture$X / fixture$sigma, fixture$cutoffs, fixture$symmetric)
  llh <- function(theta) variation_variance_loglikelihood(theta[1], theta[2], c(theta[-c(1, 2)], 1), fixture$cutoffs, fixture$symmetric, fixture$X, fixture$sigma, tpowers)
  modern <- robust_variance(stepsize, length(fixture$X), c(0.1, 0.2, 0.8), llh, seq_along(fixture$X))
  legacy <- env$RobustVariance(stepsize, length(fixture$X), c(0.1, 0.2, 0.8), llh, seq_along(fixture$X))
  expect_equal(modern, legacy)
})

test_that("estimation output matches legacy implementation", {
  env <- legacy_selection_env()
  fixture <- create_selection_fixture()
  modern <- metastudies_estimation(fixture$X, fixture$sigma, fixture$cutoffs, fixture$symmetric, model = "normal")
  legacy <- env$metastudies_estimation(fixture$X, fixture$sigma, fixture$cutoffs, fixture$symmetric, model = "normal")
  expect_equal(modern$Psihat, legacy$Psihat)
  expect_equal(modern$SE, legacy$SE)
  modern_table <- estimates_table(modern$Psihat, modern$SE, fixture$cutoffs, fixture$symmetric, "normal")
  legacy_table <- env$estimatestable(legacy$Psihat, legacy$SE, fixture$cutoffs, fixture$symmetric, "normal")
  expect_equal(modern_table, legacy_table)
})
