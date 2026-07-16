box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_named,
    expect_true,
    test_that
  ],
  artma / calc / methods / endo_kink[
    prepare_endokink_columns,
    fit_auxiliary_lm,
    compute_cutoff,
    run_endogenous_kink
  ]
)

# prepare_endokink_columns --------------------------------------------------

test_that("prepare_endokink_columns derives the regression columns exactly", {
  data <- data.frame(effect = c(0.2, 0.4, 0.6), se = c(0.1, 0.2, 0.4))
  out <- prepare_endokink_columns(data)

  expect_named(out, c("bs", "sebs", "ones", "sebs2", "wis", "bs_sebs", "ones_sebs", "bswis"))
  expect_equal(out$bs, c(0.2, 0.4, 0.6))
  expect_equal(out$sebs, c(0.1, 0.2, 0.4))
  expect_equal(out$sebs2, c(0.01, 0.04, 0.16))
  expect_equal(out$wis, 1 / c(0.01, 0.04, 0.16))
  expect_equal(out$bs_sebs, c(2.0, 2.0, 1.5))
  expect_equal(out$ones_sebs, c(10.0, 5.0, 2.5))
  expect_equal(out$bswis, c(0.2, 0.4, 0.6) / c(0.01, 0.04, 0.16))
})

# compute_cutoff ------------------------------------------------------------

test_that("compute_cutoff matches its closed form above the kink", {
  # estimate > 1.96 * sd -> (est - 1.96 sd)(est + 1.96 sd) / (2 * 1.96 * est)
  expected <- (0.5 - 1.96 * 0.1) * (0.5 + 1.96 * 0.1) / (2 * 1.96 * 0.5)
  expect_equal(compute_cutoff(0.5, 0.1), expected)
})

test_that("compute_cutoff is zero when the estimate is within the band", {
  expect_equal(compute_cutoff(0.1, 0.5), 0)
})

# fit_auxiliary_lm ----------------------------------------------------------

test_that("fit_auxiliary_lm returns the requested coefficient of a fitted lm", {
  data <- prepare_endokink_columns(
    data.frame(effect = c(0.2, 0.4, 0.6, 0.1, 0.5), se = c(0.1, 0.2, 0.4, 0.15, 0.3))
  )
  fit <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + ones, data, "ones_sebs")

  reference <- summary(stats::lm(bs_sebs ~ 0 + ones_sebs + ones, data = data))$coefficients
  expect_equal(fit$estimate, unname(reference["ones_sebs", "Estimate"]))
  expect_equal(fit$std_error, unname(reference["ones_sebs", "Std. Error"]))
  expect_true(inherits(fit$model, "lm"))
})

# run_endogenous_kink -------------------------------------------------------

test_that("run_endogenous_kink recovers a homogeneous true effect", {
  set.seed(5)
  n <- 40
  se <- runif(n, 0.05, 0.5)
  # No publication bias: effect centred on mu = 0.3 with sampling noise ~ se.
  effect <- 0.3 + rnorm(n, 0, se)

  out <- run_endogenous_kink(data.frame(effect, se), verbose = FALSE)

  expect_length(out, 4L)
  # First element is the mean-effect estimate.
  expect_equal(unname(out[1]), 0.3, tolerance = 0.1)
  expect_true(is.finite(out[2]))
})

test_that("run_endogenous_kink is deterministic for identical input", {
  set.seed(5)
  n <- 40
  se <- runif(n, 0.05, 0.5)
  effect <- 0.3 + rnorm(n, 0, se)
  df <- data.frame(effect, se)

  first <- run_endogenous_kink(df, verbose = FALSE)
  second <- run_endogenous_kink(df, verbose = FALSE)
  expect_equal(first, second)
})
