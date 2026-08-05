box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_identical,
    test_that
  ]
)

box::use(
  artma / econometric / nonlinear[run_top10, run_waap, waap_bound]
)

# Synthetic meta-analysis sample: several studies, heterogeneous effects, and
# a positive effect-se correlation so that 1/se and 1/se^2 pilot weights give
# different unrestricted means (and hence different WAAP bounds).
make_waap_data <- function() {
  set.seed(1234)
  n_studies <- 12L
  per_study <- 12L
  n <- n_studies * per_study
  study_id <- rep(paste0("S", seq_len(n_studies)), each = per_study)
  se <- stats::runif(n, 0.02, 0.5)
  study_shift <- rep(stats::rnorm(n_studies, 0, 0.05), each = per_study)
  effect <- 0.3 + 0.8 * se + study_shift + stats::rnorm(n, 0, 0.08)
  data.frame(
    study_id = study_id,
    effect = effect,
    se = se,
    stringsAsFactors = FALSE
  )
}

# Reference pilot mean: intercept of a WLS regression on a constant with
# aweight-style inverse-variance weights (Ioannidis et al. 2017).
reference_waap_bound <- function(df) {
  pilot <- stats::lm(effect ~ 1, data = df, weights = 1 / df$se^2)
  abs(stats::coef(pilot)[[1]]) / 2.8
}

# Reference WAAP/Top10 fit: WLS regression t ~ 0 + precision on the selected
# subsample, with its standard error clustered by study (HC1).
reference_wls_fit <- function(subsample) {
  t_stat <- subsample$effect / subsample$se
  precision <- 1 / subsample$se
  fit <- stats::lm(t_stat ~ 0 + precision)
  vcov <- sandwich::vcovCL(fit, cluster = subsample$study_id, type = "HC1")
  list(
    estimate = stats::coef(fit)[["precision"]],
    std_error = sqrt(vcov["precision", "precision"])
  )
}

test_that("waap_bound weights the pilot mean by inverse variance", {
  df <- make_waap_data()

  expect_equal(waap_bound(df), reference_waap_bound(df), tolerance = 1e-12)
})

test_that("WAAP selects the subsample implied by the inverse-variance pilot mean", {
  df <- make_waap_data()

  bound <- reference_waap_bound(df)
  expected_subsample <- df[df$se < bound, , drop = FALSE]

  # The old 1/se pilot weights select a different subsample, so this fixture
  # genuinely discriminates between the two weighting schemes.
  old_pilot_mean <- sum(df$effect / df$se) / sum(1 / df$se)
  old_subsample <- df[df$se < abs(old_pilot_mean) / 2.8, , drop = FALSE]
  expect_false(nrow(old_subsample) == nrow(expected_subsample))

  res <- run_waap(df, nrow(df))
  reference <- reference_wls_fit(expected_subsample)

  expect_identical(res$n_model, nrow(expected_subsample))
  expect_equal(res$effect$estimate, reference$estimate, tolerance = 1e-12)
})

test_that("WAAP reports the study-clustered WLS regression standard error", {
  df <- make_waap_data()

  expected_subsample <- df[df$se < reference_waap_bound(df), , drop = FALSE]
  reference <- reference_wls_fit(expected_subsample)

  res <- run_waap(df, nrow(df))

  expect_equal(res$effect$estimate, reference$estimate, tolerance = 1e-12)
  expect_equal(res$effect$std_error, reference$std_error, tolerance = 1e-12)

  # The homogeneous fixed-effect formula understates the uncertainty here.
  fixed_effect_se <- sqrt(1 / sum(1 / expected_subsample$se^2))
  expect_gt(res$effect$std_error, fixed_effect_se)
})

test_that("Top10 reports the study-clustered WLS regression standard error", {
  df <- make_waap_data()

  precision <- 1 / df$se
  threshold <- stats::quantile(precision, probs = 0.9, names = FALSE)
  expected_subsample <- df[precision > threshold, , drop = FALSE]
  reference <- reference_wls_fit(expected_subsample)

  res <- run_top10(df, nrow(df))

  expect_identical(res$n_model, nrow(expected_subsample))
  expect_equal(res$effect$estimate, reference$estimate, tolerance = 1e-12)
  expect_equal(res$effect$std_error, reference$std_error, tolerance = 1e-12)

  fixed_effect_se <- sqrt(1 / sum(1 / expected_subsample$se^2))
  expect_gt(res$effect$std_error, fixed_effect_se)
})
