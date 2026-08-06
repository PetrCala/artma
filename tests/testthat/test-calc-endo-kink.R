box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_length,
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

  expect_length(out, 5L)
  # First element is the mean-effect estimate.
  expect_equal(unname(out[1]), 0.3, tolerance = 0.1)
  expect_true(is.finite(out[2]))
  # 5th element is the heterogeneity standard deviation: finite and >= 0.
  expect_true(is.finite(out[5]) && out[5] >= 0)
})

test_that("run_endogenous_kink matches the Bom & Rachinger reference on low-heterogeneity data", {
  # Regression test for issue #366: the heterogeneity variance divisor must be
  # M - model_df - 1 (model df = 2 regressors), not M - df.residual - 1 = 1.
  # The inflated variance made the interior-kink condition never fire, so the
  # estimator silently degenerated to PET. Low-heterogeneity data exposes the
  # bug; the reference formula below is implemented independently.
  set.seed(42)
  n <- 200
  se <- runif(n, 0.05, 0.5)
  effect <- 0.5 + rnorm(n, 0, 0.05)

  ref <- data.frame(t = effect / se, prec = 1 / se, ones = 1, se = se)
  pet <- stats::lm(t ~ 0 + prec + ones, data = ref)
  peese <- stats::lm(t ~ 0 + prec + se, data = ref)
  pet_tab <- summary(pet)$coefficients
  t_stat <- pet_tab["prec", "Estimate"] / pet_tab["prec", "Std. Error"]
  if (abs(t_stat) > stats::qt(0.975, n - 2)) {
    combined <- unname(stats::coef(peese)["prec"])
    q1 <- sum(stats::residuals(peese)^2)
  } else {
    combined <- unname(stats::coef(pet)["prec"])
    q1 <- sum(stats::residuals(pet)^2)
  }
  sigma_hat <- sqrt(max(0, n * (q1 / (n - 2 - 1) - 1) / sum(1 / se^2)))
  a1 <- (combined - 1.96 * sigma_hat) * (combined + 1.96 * sigma_hat) / (2 * 1.96 * combined)
  # The kink must be interior on this data, otherwise the test loses its power.
  expect_true(combined > 1.96 * sigma_hat)
  expect_true(a1 > min(se) && a1 < max(se))
  ref$pubbias <- pmax(se - a1, 0) / se
  kinked <- summary(stats::lm(t ~ 0 + prec + pubbias, data = ref))$coefficients

  out <- run_endogenous_kink(data.frame(effect, se), verbose = FALSE)

  expect_equal(unname(out[1]), unname(kinked["prec", "Estimate"]))
  expect_equal(unname(out[2]), unname(kinked["prec", "Std. Error"]))
  expect_equal(unname(out[3]), unname(kinked["pubbias", "Estimate"]))
  expect_equal(unname(out[4]), unname(kinked["pubbias", "Std. Error"]))
  # An interior kink means the estimate is not the plain PET estimate.
  expect_false(isTRUE(all.equal(unname(out[1]), unname(stats::coef(pet)["prec"]))))
})

test_that("run_endogenous_kink's no-kink fallback matches a precision-weighted OLS fit", {
  # On heavily heterogeneous data the heterogeneity SD swamps the combined
  # estimate, compute_cutoff() returns 0, and final_endokink_fit() falls back
  # to an unrestricted regression of bs_sebs on ones_sebs + pub_bias, which is
  # algebraically the same as a weighted least-squares fit of effect on se
  # with weights 1/se^2 (i.e. "precision weighted" when a dataset's precision
  # column is defined as 1/se). A coincidental match between Endogenous Kink
  # and a precision-weighted OLS spec in this regime is therefore expected,
  # not a wiring bug.
  set.seed(7)
  n <- 150
  se <- runif(n, 0.05, 0.5)
  effect <- 0.3 + rnorm(n, 0, 0.6) + rnorm(n, 0, se)
  df <- data.frame(effect, se)

  out <- run_endogenous_kink(df, verbose = FALSE)
  reference <- summary(stats::lm(effect ~ se, data = df, weights = 1 / se^2))$coefficients

  expect_equal(unname(out[1]), unname(reference["(Intercept)", "Estimate"]))
  expect_equal(unname(out[2]), unname(reference["(Intercept)", "Std. Error"]))
  expect_equal(unname(out[3]), unname(reference["se", "Estimate"]))
  expect_equal(unname(out[4]), unname(reference["se", "Std. Error"]))
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
