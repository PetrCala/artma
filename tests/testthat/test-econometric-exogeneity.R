box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options],
  artma / econometric / exogeneity[
    run_iv_regression,
    run_puniform_star,
    find_best_instrument,
    run_exogeneity_tests
  ]
)

# A meta-analysis DGP with a known effect and a valid instrument.
#
#   se_i        = 2 / sqrt(n_obs_i) + noise      (relevance: se depends on n_obs)
#   effect_i    = mu + bias * se_i + eps_i       (mu and bias are recoverable)
#
# An IV regression of effect on se, instrumented by a function of n_obs, should
# recover the intercept (effect beyond bias ~ mu) and the slope (publication
# bias ~ bias).
make_exogeneity_df <- function(seed = 2024, n = 300, mu = 0.5, bias = 1.0) {
  set.seed(seed)
  n_obs <- sample(30:600, n, replace = TRUE)
  se <- 2 / sqrt(n_obs) + abs(rnorm(n, 0, 0.01))
  effect <- mu + bias * se + rnorm(n, 0, 0.05)
  data.frame(
    effect = effect,
    se = se,
    study_id = rep(seq_len(60), length.out = n),
    n_obs = n_obs,
    study_size = n_obs
  )
}

default_exogeneity_options <- function(...) {
  defaults <- list(
    iv_instrument = "automatic",
    add_significance_marks = TRUE,
    round_to = 3L,
    puniform_alpha = 0.05,
    puniform_method = "ML"
  )
  utils::modifyList(defaults, list(...))
}

# run_iv_regression ---------------------------------------------------------

test_that("run_iv_regression recovers the effect and bias from a known DGP", {
  skip_if_not_installed("AER")
  skip_if_not_installed("ivmodel")
  local_options(artma.verbose = 1)

  df <- make_exogeneity_df()
  res <- run_iv_regression(df, iv_instrument = "1/sqrt(n_obs)")

  expect_equal(res$coefficients$term, c("effect", "publication_bias"))
  effect_est <- res$coefficients$estimate[res$coefficients$term == "effect"]
  bias_est <- res$coefficients$estimate[res$coefficients$term == "publication_bias"]

  # Recovers mu = 0.5 and bias = 1.0 within sampling error.
  expect_equal(effect_est, 0.5, tolerance = 0.05)
  expect_equal(bias_est, 1.0, tolerance = 0.2)
  # Strong instrument: Anderson-Rubin F well above conventional weak thresholds.
  expect_true(res$ar_fstat > 30)
})

test_that("run_iv_regression auto-selects the sample-size instrument", {
  skip_if_not_installed("AER")
  skip_if_not_installed("ivmodel")
  local_options(artma.verbose = 1)

  df <- make_exogeneity_df()
  res <- run_iv_regression(df, iv_instrument = "automatic")

  expect_equal(res$instrument_name, "1/sqrt(n_obs)")
  expect_equal(nrow(res$coefficients), 2L)
})

test_that("run_iv_regression rejects an instrument without n_obs", {
  skip_if_not_installed("AER")
  df <- make_exogeneity_df()
  expect_error(run_iv_regression(df, iv_instrument = "1/sqrt(study_size)"))
})

# find_best_instrument ------------------------------------------------------

test_that("find_best_instrument returns one of the candidate instruments", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  df <- make_exogeneity_df()
  instruments <- list(1 / sqrt(df$n_obs), 1 / df$n_obs, log(df$n_obs))
  names_verbose <- c("1/sqrt(n_obs)", "1/n_obs", "log(n_obs)")

  best <- find_best_instrument(df, instruments, names_verbose)
  expect_true(all(best %in% names_verbose))
})

# run_puniform_star ---------------------------------------------------------

test_that("run_puniform_star returns NA estimates without enough significant studies", {
  set.seed(7)
  n <- 40
  # z-scores well below 1.96: no study is significant, so the estimator bails.
  df <- data.frame(
    effect = rnorm(n, 0.001, 0.001),
    se = rep(1, n),
    study_id = rep(seq_len(10), length.out = n),
    study_size = sample(20:50, n, replace = TRUE),
    n_obs = sample(20:50, n, replace = TRUE)
  )

  res <- run_puniform_star(df)

  expect_equal(res$coefficients$term, c("effect", "publication_bias_test"))
  expect_true(is.na(res$coefficients$estimate[1]))
  expect_true(is.na(res$test_p_value))
})

test_that("run_puniform_star returns a finite estimate with significant studies", {
  set.seed(11)
  n <- 120
  df <- data.frame(
    effect = rnorm(n, 0.4, 0.03),
    se = rep(0.05, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = sample(50:200, n, replace = TRUE),
    n_obs = sample(50:200, n, replace = TRUE)
  )

  res <- run_puniform_star(df)
  expect_true(is.finite(res$coefficients$estimate[1]))
})

# run_exogeneity_tests ------------------------------------------------------

test_that("run_exogeneity_tests assembles IV and p-uniform results", {
  skip_if_not_installed("AER")
  skip_if_not_installed("ivmodel")
  local_options(artma.verbose = 1)

  df <- make_exogeneity_df()
  res <- run_exogeneity_tests(df, default_exogeneity_options())

  expect_true(all(c("iv", "puniform", "summary") %in% names(res)))
  expect_true(is.data.frame(res$summary))
  expect_equal(nrow(res$summary), 6L)
  expect_true("IV" %in% colnames(res$summary))

  effect_est <- res$iv$coefficients$estimate[res$iv$coefficients$term == "effect"]
  expect_equal(effect_est, 0.5, tolerance = 0.05)
})

test_that("run_exogeneity_tests skips gracefully when columns are missing", {
  skip_if_not_installed("AER")
  skip_if_not_installed("ivmodel")
  local_options(artma.verbose = 1)

  res <- run_exogeneity_tests(data.frame(effect = 1:3, se = rep(1, 3)), default_exogeneity_options())

  expect_true(!is.null(res$skipped))
  expect_true(grepl("study_id", res$skipped$reason))
})

test_that("run_exogeneity_tests aborts when a required package is absent", {
  local_options(artma.verbose = 1)
  df <- make_exogeneity_df(n = 30)

  local_pretend_packages_absent("AER")
  expect_error(run_exogeneity_tests(df, default_exogeneity_options()), regexp = "AER")
})

test_that("run_exogeneity_tests aborts when ivmodel is absent", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)
  df <- make_exogeneity_df(n = 30)

  local_pretend_packages_absent("ivmodel")
  expect_error(run_exogeneity_tests(df, default_exogeneity_options()), regexp = "ivmodel")
})
