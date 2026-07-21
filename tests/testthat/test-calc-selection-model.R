box::use(
  testthat[
    expect_equal,
    expect_length,
    expect_true,
    test_that
  ],
  artma / calc / methods / selection_model[
    compute_tpowers,
    variation_variance_loglikelihood,
    clustered_covariance_estimate,
    estimates_table,
    metastudies_estimation
  ]
)

# compute_tpowers -----------------------------------------------------------

test_that("compute_tpowers builds symmetric significance indicators", {
  tp <- compute_tpowers(c(0.5, 1.5, 2.5), cutoffs = 1.96, symmetric = TRUE)
  # column 1: |t| < 1.96; column 2: |t| >= 1.96
  expect_equal(tp[, 1], c(1, 1, 0))
  expect_equal(tp[, 2], c(0, 0, 1))
})

test_that("compute_tpowers keeps the sign when asymmetric", {
  tp <- compute_tpowers(c(-2.5, 0.5, 2.5), cutoffs = 1.96, symmetric = FALSE)
  # column 1: t < 1.96 (true for -2.5 and 0.5); column 2: t >= 1.96
  expect_equal(tp[, 1], c(1, 1, 0))
  expect_equal(tp[, 2], c(0, 0, 1))
})

test_that("compute_tpowers partitions multiple cutoffs into adjacent bins", {
  tp <- compute_tpowers(c(0.5, 1.2, 2.0), cutoffs = c(1, 1.64), symmetric = TRUE)
  # bins: [0,1), [1,1.64), [1.64, Inf)
  expect_equal(tp[, 1], c(1, 0, 0))
  expect_equal(tp[, 2], c(0, 1, 0))
  expect_equal(tp[, 3], c(0, 0, 1))
  # Each row is a partition (exactly one bin).
  expect_equal(rowSums(tp), c(1, 1, 1))
})

# variation_variance_loglikelihood ------------------------------------------

test_that("variation_variance_loglikelihood aggregates the per-observation logs", {
  set.seed(1)
  x <- rnorm(8, 0.3, 0.1)
  sigma <- rep(0.1, 8)
  tp <- compute_tpowers(x / sigma, cutoffs = 1.96, symmetric = TRUE)

  ll <- variation_variance_loglikelihood(0.3, 0.05, c(1, 1), 1.96, TRUE, x, sigma, tp)

  expect_length(ll$logL, 8L)
  expect_true(is.finite(ll$LLH))
  # LLH is the negated sum of the individual log-likelihoods.
  expect_equal(ll$LLH, -sum(ll$logL))
})

# clustered_covariance_estimate ---------------------------------------------

test_that("clustered_covariance_estimate returns a symmetric p-by-p matrix", {
  set.seed(3)
  scores <- matrix(rnorm(30), ncol = 3)
  clusters <- rep(seq_len(5), each = 2)

  cov <- clustered_covariance_estimate(scores, clusters)

  expect_equal(dim(cov), c(3L, 3L))
  expect_equal(cov, t(cov))
})

# estimates_table -----------------------------------------------------------

test_that("estimates_table lays out estimates and standard errors", {
  psi <- c(0.3, 0.1, 0.8)
  se <- c(0.05, 0.02, 0.1)

  tab <- estimates_table(psi, se, cutoffs = 1.96, symmetric = TRUE, model = "normal")

  expect_equal(rownames(tab), c("estimate", "standard error"))
  expect_equal(unname(tab["estimate", ]), psi)
  expect_equal(unname(tab["standard error", ]), se)
  # mu and tau are the first two column labels.
  expect_equal(colnames(tab)[1], intToUtf8(956))
  expect_equal(colnames(tab)[2], intToUtf8(964))
})

# metastudies_estimation ----------------------------------------------------

test_that("metastudies_estimation recovers a homogeneous true effect", {
  set.seed(5)
  n <- 40
  se <- runif(n, 0.05, 0.5)
  effect <- 0.3 + rnorm(n, 0, se)

  fit <- metastudies_estimation(effect, se, cutoffs = 1.96, symmetric = TRUE, model = "normal")

  # Psihat = c(mu, tau, publication weight); mu is the mean effect.
  expect_equal(fit$Psihat[1], 0.3, tolerance = 0.1)
  expect_length(fit$SE, length(fit$Psihat))
  expect_true(all(fit$Psihat[-1] >= 0))
})
