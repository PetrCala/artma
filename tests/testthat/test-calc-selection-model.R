box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_length,
    expect_message,
    expect_true,
    test_that
  ],
  artma / calc / methods / selection_model[
    compute_tpowers,
    variation_variance_loglikelihood,
    clustered_covariance_estimate,
    compute_information_matrix,
    compute_score_matrix,
    estimates_table,
    metastudies_estimation
  ]
)

# Synthetic meta-analytic sample with genuine study-level heterogeneity, so
# study clustering changes the standard errors and the selection model's
# publication-probability parameters sit in the interior of their cells.
make_clustered_selection_data <- function() {
  set.seed(2024)
  n_studies <- 15L
  per_study <- 6L
  n <- n_studies * per_study
  study <- rep(seq_len(n_studies), each = per_study)
  study_effect <- rnorm(n_studies, mean = 0.1, sd = 0.3)
  se <- runif(n, 0.05, 0.35)
  effect <- study_effect[study] + rnorm(n, 0, se)
  list(effect = effect, se = se, study = study, n = n)
}

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

test_that("metastudies_estimation supports zero and negative cutoffs", {
  d <- make_clustered_selection_data()

  fit <- metastudies_estimation(
    d$effect, d$se,
    cutoffs = c(-1.96, 0, 1.96), symmetric = FALSE, model = "normal",
    cluster_id = d$study
  )

  # mu, tau, and one publication probability per interval below a cutoff.
  expect_length(fit$Psihat, 5L)
  expect_equal(fit$convergence, 0)
  expect_false(fit$boundary_hit)
})

test_that("metastudies_estimation clusters standard errors by study", {
  d <- make_clustered_selection_data()

  fit_clustered <- metastudies_estimation(
    d$effect, d$se,
    cutoffs = 1.96, symmetric = FALSE, model = "normal",
    cluster_id = d$study
  )
  fit_unclustered <- metastudies_estimation(
    d$effect, d$se,
    cutoffs = 1.96, symmetric = FALSE, model = "normal"
  )

  # Clustering only changes the variance estimate, not the point estimates.
  expect_equal(fit_clustered$Psihat, fit_unclustered$Psihat)
  expect_true(fit_clustered$clustered)
  expect_false(fit_unclustered$clustered)
  expect_false(isTRUE(all.equal(fit_clustered$SE, fit_unclustered$SE)))
})

test_that("clustered standard errors match an independently computed cluster sandwich", {
  d <- make_clustered_selection_data()

  fit <- metastudies_estimation(
    d$effect, d$se,
    cutoffs = 1.96, symmetric = FALSE, model = "normal",
    cluster_id = d$study
  )

  tpowers <- compute_tpowers(d$effect / d$se, cutoffs = 1.96, symmetric = FALSE)
  llh <- function(psi) {
    variation_variance_loglikelihood(
      psi[1], psi[2], c(psi[3], 1), 1.96, FALSE, d$effect, d$se, tpowers
    )
  }
  scores <- compute_score_matrix(fit$Psihat, 1e-6, llh)
  centered <- sweep(scores, 2, colMeans(scores))
  meat <- crossprod(rowsum(centered, d$study)) / (d$n - 1)
  bread <- solve(compute_information_matrix(fit$Psihat, 1e-6, llh))
  expected_se <- sqrt(diag(bread %*% meat %*% bread * d$n))

  expect_equal(unname(fit$SE), unname(expected_se), tolerance = 1e-8)
})

test_that("metastudies_estimation falls back to unclustered errors with a single cluster", {
  d <- make_clustered_selection_data()

  fit_single <- NULL
  expect_message(
    fit_single <- metastudies_estimation(
      d$effect, d$se,
      cutoffs = 1.96, symmetric = FALSE, model = "normal",
      cluster_id = rep(1L, d$n)
    ),
    "unclustered"
  )
  fit_unclustered <- metastudies_estimation(
    d$effect, d$se,
    cutoffs = 1.96, symmetric = FALSE, model = "normal"
  )

  expect_false(fit_single$clustered)
  expect_equal(fit_single$SE, fit_unclustered$SE)
})
