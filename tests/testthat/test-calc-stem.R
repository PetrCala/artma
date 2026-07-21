box::use(
  testthat[
    expect_equal,
    expect_true,
    test_that
  ],
  artma / calc / methods / stem[
    weighted_mean,
    weighted_mean_squared,
    variance_b,
    variance_0,
    stem
  ]
)

# weighted_mean -------------------------------------------------------------

test_that("weighted_mean is the running inverse-variance weighted mean", {
  # Equal weights (se = 1, sigma = 0): cumulative arithmetic mean.
  expect_equal(weighted_mean(c(1, 2, 3), c(1, 1, 1), 0), c(1, 1.5, 2))
})

test_that("weighted_mean honours unequal precisions", {
  beta <- c(1, 3)
  se <- c(1, 2)
  sigma <- 0
  weights <- 1 / se^2
  expected <- c(beta[1], sum(beta * weights) / sum(weights))
  expect_equal(weighted_mean(beta, se, sigma), expected)
})

# variance_b ----------------------------------------------------------------

test_that("variance_b is the reciprocal cumulative precision", {
  expect_equal(variance_b(c(1, 1, 1), 0), 1 / cumsum(c(1, 1, 1)))
  expect_equal(variance_b(c(1, 1, 1), 0), c(1, 0.5, 1 / 3))
})

# variance_0 ----------------------------------------------------------------

test_that("variance_0 returns zero when dispersion matches sampling error", {
  # sum(w * (beta - mean)^2) = q - 1 degrees of freedom cancels the numerator.
  expect_equal(variance_0(3, c(1, 2, 3), c(1, 1, 1), 2), 0)
})

test_that("variance_0 matches its closed form with excess dispersion", {
  beta <- c(0, 2, 4)
  se <- c(1, 1, 1)
  mean_val <- 2
  # weights = 1; numerator = sum((beta-mean)^2) - (n-1) = 8 - 2 = 6
  # denominator = sum(w) - sum(w^2)/sum(w) = 3 - 1 = 2 -> 3
  expect_equal(variance_0(3, beta, se, mean_val), 3)
})

# weighted_mean_squared ------------------------------------------------------

test_that("weighted_mean_squared matches the outer-product reference", {
  set.seed(7)
  n <- 25
  se <- runif(n, 0.05, 0.5)
  beta <- 0.3 + rnorm(n, 0, se)
  sigma <- 0.1

  weights <- 1 / (se^2 + sigma^2)
  weighted_beta <- beta * weights
  submatrix_sums <- function(m) {
    vapply(seq_len(nrow(m)), function(i) sum(m[1:i, 1:i]), numeric(1))
  }
  numerator <- submatrix_sums(outer(weighted_beta, weighted_beta)) - cumsum(weighted_beta^2)
  denominator <- submatrix_sums(outer(weights, weights)) - cumsum(weights^2)
  expected <- numerator / denominator
  expected[1] <- 0

  expect_equal(weighted_mean_squared(beta, se, sigma), expected)
})

# stem ----------------------------------------------------------------------

test_that("stem recovers a homogeneous true effect", {
  set.seed(5)
  n <- 40
  se <- runif(n, 0.05, 0.5)
  effect <- 0.3 + rnorm(n, 0, se)

  out <- stem(effect, se, c(1e-4, 100))

  expect_true(is.matrix(out$estimates))
  expect_equal(unname(out$estimates[1, "estimate"]), 0.3, tolerance = 0.1)
  expect_true(out$estimates[1, "se"] > 0)
  # The stem uses a subset of at least the three most precise studies.
  expect_true(out$estimates[1, "n_stem"] >= 3)
  expect_true(out$estimates[1, "n_stem"] <= n)
})

test_that("stem is deterministic for identical input", {
  set.seed(5)
  n <- 40
  se <- runif(n, 0.05, 0.5)
  effect <- 0.3 + rnorm(n, 0, se)

  expect_equal(stem(effect, se, c(1e-4, 100)), stem(effect, se, c(1e-4, 100)))
})
