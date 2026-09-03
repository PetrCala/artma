box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_gt,
    expect_lt,
    expect_match,
    expect_message,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options],
  artma / econometric / exogeneity[
    build_exogeneity_summary,
    run_iv_regression,
    run_puniform_star,
    find_best_instrument,
    run_exogeneity_tests,
    WEAK_INSTRUMENT_F_THRESHOLD
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
make_exogeneity_df <- function(seed = 4, n = 120, mu = 0.5, bias = 1.0) {
  set.seed(seed)
  n_obs <- sample(30:600, n, replace = TRUE)
  se <- 2 / sqrt(n_obs) + abs(rnorm(n, 0, 0.01))
  effect <- mu + bias * se + rnorm(n, 0, 0.05)
  data.frame(
    effect = effect,
    se = se,
    study_id = rep(seq_len(24), length.out = n),
    n_obs = n_obs,
    study_size = n_obs
  )
}

# A DGP where se is essentially unrelated to n_obs, so any instrument built
# from n_obs is a weak predictor of se in the first stage.
make_weak_instrument_df <- function(seed = 99, n = 120, mu = 0.5, bias = 1.0) {
  set.seed(seed)
  n_obs <- sample(30:600, n, replace = TRUE)
  se <- 0.1 + abs(rnorm(n, 0, 0.05))
  effect <- mu + bias * se + rnorm(n, 0, 0.05)
  data.frame(
    effect = effect,
    se = se,
    study_id = rep(seq_len(24), length.out = n),
    n_obs = n_obs,
    study_size = n_obs
  )
}

default_exogeneity_options <- function(...) {
  defaults <- list(
    iv_instrument = "1/sqrt(n_obs)",
    add_significance_marks = TRUE,
    round_to = 3L,
    puniform_alpha = 0.05,
    puniform_method = "ML",
    puniform_side = "auto"
  )
  utils::modifyList(defaults, list(...))
}

# run_iv_regression ---------------------------------------------------------

test_that("run_iv_regression recovers the effect and bias from a known DGP", {
  skip_if_not_installed("AER")
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
  expect_true(res$first_stage_fstat > WEAK_INSTRUMENT_F_THRESHOLD)
  expect_false(res$weak_instrument)
})

test_that("run_iv_regression defaults to the a-priori sample-size instrument", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  # The default is fixed rather than selected: ranking candidates by their
  # first-stage F on the same data used for inference is a specification
  # search, so "automatic" is opt-in only.
  df <- make_exogeneity_df()
  res <- run_iv_regression(df)

  expect_equal(res$instrument_name, "1/sqrt(n_obs)")
  expect_equal(
    res$coefficients$estimate,
    run_iv_regression(df, iv_instrument = "1/sqrt(n_obs)")$coefficients$estimate
  )
})

test_that("the iv_instrument template default matches the method's opt_spec", {
  # CLAUDE.md: an opt_spec default that drifts from the template default makes
  # getOption() and the options file disagree about what a fresh run does.
  tpl <- yaml::read_yaml(
    system.file("artma/options/templates/options_template.yaml", package = "artma")
  )
  expect_equal(tpl$methods$exogeneity_tests$iv_instrument$default, "1/sqrt(n_obs)")
})

test_that("run_iv_regression still auto-selects when explicitly asked", {
  skip_if_not_installed("AER")
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

test_that("run_iv_regression warns and flags a weak instrument", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  df <- make_weak_instrument_df()
  expect_message(
    res <- run_iv_regression(df, iv_instrument = "1/sqrt(n_obs)"),
    "Weak instrument"
  )

  expect_true(res$weak_instrument)
  expect_true(res$first_stage_fstat < WEAK_INSTRUMENT_F_THRESHOLD)
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

test_that("find_best_instrument selects by first-stage F, not Wu-Hausman or R-squared", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  # se depends on n_obs, so 1/sqrt(n_obs) is a strong instrument; log(n_obs) is
  # a much weaker predictor of se and would previously have been able to win
  # a majority vote via the (backwards) Wu-Hausman or R-squared criteria.
  df <- make_exogeneity_df()
  instruments <- list(1 / sqrt(df$n_obs), log(df$n_obs))
  names_verbose <- c("1/sqrt(n_obs)", "log(n_obs)")

  best <- find_best_instrument(df, instruments, names_verbose)
  expect_equal(best, "1/sqrt(n_obs)")
})

test_that("find_best_instrument breaks ties in favor of 1/sqrt(n_obs)", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  # A rescaled copy of the same instrument has an identical first-stage F,
  # so this is a genuine tie that the tie-break rule must resolve.
  df <- make_exogeneity_df()
  instruments <- list(1 / sqrt(df$n_obs), 2 / sqrt(df$n_obs))
  names_verbose <- c("1/sqrt(n_obs)", "rescaled")

  best <- find_best_instrument(df, instruments, names_verbose)
  expect_equal(best, "1/sqrt(n_obs)")
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

test_that("run_puniform_star recovers the effect when n_obs is large", {
  # Regression test: the study variance used to be computed as se^2 * n_obs
  # (the variance of the underlying micro observations, not of the effect
  # estimate), so large primary-study samples flattened the likelihood and
  # dragged the estimate far below the true effect. With vi = se^2 the
  # estimate must stay in the neighborhood of the simulated truth.
  set.seed(11)
  n <- 120
  df <- data.frame(
    effect = rnorm(n, 5, 0.5),
    se = rep(0.5, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = rep(5000L, n),
    n_obs = rep(5000L, n)
  )

  res <- run_puniform_star(df, method = "ML")
  eff <- res$coefficients[res$coefficients$term == "effect", ]

  expect_equal(res$method_used, "ML")
  expect_gt(eff$estimate, 4)
  expect_lt(eff$estimate, 6)
})

test_that("run_puniform_star with method = 'P' returns a finite, positive estimate with significant studies", {
  set.seed(11)
  n <- 120
  # A large effect relative to the per-observation SD (se * sqrt(n_obs)) so that
  # studies are significant on the same scale the p-uniform transform uses.
  df <- data.frame(
    effect = rnorm(n, 3, 0.05),
    se = rep(0.05, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = rep(50, n),
    n_obs = rep(50, n)
  )

  res_mm <- run_puniform_star(df, method = "P")

  expect_true(is.finite(res_mm$coefficients$estimate[1]))
  expect_true(res_mm$coefficients$estimate[1] > 0)
  expect_true(is.finite(res_mm$test_p_value))
})

test_that("run_puniform_star with method = 'P' returns NA estimates without enough significant studies", {
  set.seed(7)
  n <- 40
  df <- data.frame(
    effect = rnorm(n, 0.001, 0.001),
    se = rep(1, n),
    study_id = rep(seq_len(10), length.out = n),
    study_size = sample(20:50, n, replace = TRUE),
    n_obs = sample(20:50, n, replace = TRUE)
  )

  res <- run_puniform_star(df, method = "P")

  expect_true(is.na(res$coefficients$estimate[1]))
  expect_true(is.na(res$test_p_value))
})

test_that("publication-bias test does not flag a bias-free literature with a real effect", {
  # Regression test: the statistic used to be a test of theta = 0 (no effect),
  # so any genuine nonzero effect was reported as strong "publication bias".
  # The Fisher-type test at the fixed-effect estimate must stay quiet here:
  # every study is drawn from the same normal model with no selection.
  set.seed(11)
  n <- 120
  df <- data.frame(
    effect = rnorm(n, 0.4, 0.03),
    se = rep(0.05, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = sample(50:200, n, replace = TRUE),
    n_obs = sample(50:200, n, replace = TRUE)
  )

  res <- run_puniform_star(df, method = "ML")
  test_row <- res$coefficients[res$coefficients$term == "publication_bias_test", ]

  expect_true(is.finite(test_row$statistic))
  expect_true(is.finite(test_row$p_value))
  expect_gt(test_row$p_value, 0.05)
})

test_that("publication-bias test flags a literature censored at significance", {
  # Simulate one-directional selective reporting: draw a null-effect
  # literature and keep only studies significantly POSITIVE. The naive
  # fixed-effect estimate is then inflated above the truth, and the
  # conditional transforms evaluated at it deviate from uniformity.
  set.seed(21)
  n <- 4000
  effect <- rnorm(n, 0, 0.06)
  se <- rep(0.05, n)
  keep <- effect / se >= stats::qnorm(0.975)
  kept <- which(keep)[seq_len(80)]
  df <- data.frame(
    effect = effect[kept],
    se = se[kept],
    study_id = seq_along(kept),
    study_size = rep(100L, length(kept)),
    n_obs = rep(100L, length(kept))
  )

  res <- run_puniform_star(df, method = "P")
  test_row <- res$coefficients[res$coefficients$term == "publication_bias_test", ]

  expect_true(is.finite(test_row$p_value))
  expect_lt(test_row$p_value, 0.05)
})

test_that("run_puniform_mm recovers negative effects", {
  # Regression test: the root search used to cover [0, upper] only, so
  # meta-analyses of genuinely negative effects returned "not estimable".
  set.seed(13)
  n <- 120
  df <- data.frame(
    effect = rnorm(n, -3, 0.05),
    se = rep(0.05, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = rep(50, n),
    n_obs = rep(50, n)
  )

  res <- run_puniform_star(df, method = "P")
  eff <- res$coefficients[res$coefficients$term == "effect", ]

  expect_true(is.finite(eff$estimate))
  expect_lt(eff$estimate, -2)
})

test_that("run_puniform_star falls back to method 'P' when ML does not converge", {
  testthat::local_mocked_bindings(
    optim = function(...) list(par = c(NA_real_, NA_real_), value = NA_real_, convergence = 1),
    .package = "stats"
  )

  set.seed(11)
  n <- 120
  df <- data.frame(
    effect = rnorm(n, 3, 0.05),
    se = rep(0.05, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = rep(50, n),
    n_obs = rep(50, n)
  )

  res <- run_puniform_star(df, method = "ML")

  expect_equal(res$method_used, "P")
  expect_true(is.finite(res$coefficients$estimate[1]))
  expect_match(res$note, "did not converge")
  expect_match(res$note, "Fell back to the method-of-moments")
})

test_that("run_puniform_star reports a note when the ML Hessian is not invertible", {
  set.seed(11)
  n <- 120
  df <- data.frame(
    effect = rnorm(n, 0.4, 0.03),
    se = rep(0.05, n),
    study_id = rep(seq_len(30), each = 4),
    study_size = sample(50:200, n, replace = TRUE),
    n_obs = sample(50:200, n, replace = TRUE)
  )

  res <- run_puniform_star(df, method = "ML")

  expect_equal(res$method_used, "ML")
  expect_true(is.na(res$coefficients$std_error[1]))
  expect_match(res$note, "Hessian was not invertible")
})

# A heterogeneous literature with a roughly even split of significant and
# non-significant studies, so the p-uniform* likelihood has both kinds of
# truncated-normal contributions to work with.
make_puniform_star_df <- function(seed = 519, k = 40) {
  set.seed(seed)
  se <- runif(k, 0.05, 0.25)
  true_effect <- rnorm(k, 0.25, 0.12)
  effect <- rnorm(k, true_effect, se)
  data.frame(
    effect = effect,
    se = se,
    study_id = seq_len(k),
    study_size = rep(100L, k),
    n_obs = rep(100L, k)
  )
}

test_that("run_puniform_star ML matches puniform::puni_star on a mixed literature", {
  # Regression test: the likelihood used to be fitted on the significant
  # studies only (the original p-uniform), while the method is labelled
  # p-uniform*, whose whole point is to also use the non-significant ones.
  skip_if_not_installed("puniform")

  df <- make_puniform_star_df()
  n_sig <- sum(df$effect / df$se >= stats::qnorm(0.975))
  expect_gt(n_sig, 5)
  expect_lt(n_sig, nrow(df) - 5)

  res <- run_puniform_star(df, method = "ML", side = "right")
  ref <- puniform::puni_star(yi = df$effect, vi = df$se^2, side = "right", method = "ML", alpha = 0.05)

  expect_equal(res$method_used, "ML")
  expect_equal(res$coefficients$estimate[1], ref$est, tolerance = 1e-4)
})

test_that("run_puniform_star ML uses the non-significant studies", {
  df <- make_puniform_star_df()
  base <- run_puniform_star(df, method = "ML", side = "right")

  # Shift only the non-significant studies; a significant-only fit would not
  # notice, the star likelihood must.
  shifted <- df
  non_sig <- shifted$effect / shifted$se < stats::qnorm(0.975)
  shifted$effect[non_sig] <- shifted$effect[non_sig] - 0.1
  moved <- run_puniform_star(shifted, method = "ML", side = "right")

  expect_true(is.finite(base$coefficients$estimate[1]))
  expect_true(is.finite(moved$coefficients$estimate[1]))
  expect_lt(moved$coefficients$estimate[1], base$coefficients$estimate[1] - 0.05)
})

test_that("run_puniform_star side = 'left' mirrors side = 'right' on negated data", {
  df <- make_puniform_star_df()
  negated <- df
  negated$effect <- -negated$effect

  for (method in c("ML", "P")) {
    right <- run_puniform_star(df, method = method, side = "right")
    left <- run_puniform_star(negated, method = method, side = "left")

    expect_equal(left$side_used, "left")
    expect_equal(left$coefficients$estimate[1], -right$coefficients$estimate[1], tolerance = 1e-6)
    expect_equal(left$coefficients$std_error[1], right$coefficients$std_error[1], tolerance = 1e-6)
    expect_equal(left$test_statistic, right$test_statistic, tolerance = 1e-6)
  }
})

test_that("run_puniform_star side = 'auto' follows the sign of the pooled effect", {
  df <- make_puniform_star_df()
  negated <- df
  negated$effect <- -negated$effect

  auto_pos <- run_puniform_star(df, method = "ML")
  auto_neg <- run_puniform_star(negated, method = "ML")

  expect_equal(auto_pos$side_used, "right")
  expect_equal(auto_neg$side_used, "left")
  expect_equal(auto_pos$coefficients$estimate[1], run_puniform_star(df, method = "ML", side = "right")$coefficients$estimate[1])
  expect_equal(auto_neg$coefficients$estimate[1], -auto_pos$coefficients$estimate[1], tolerance = 1e-6)
})

test_that("run_puniform_star reports the side when the declared side has no significant studies", {
  df <- make_puniform_star_df()

  res <- run_puniform_star(df, method = "ML", side = "left")

  expect_equal(res$side_used, "left")
  expect_true(is.na(res$coefficients$estimate[1]))
  expect_match(res$note, "on the left side")
})

test_that("run_puniform_star rejects an unknown side", {
  df <- make_puniform_star_df()
  expect_error(run_puniform_star(df, side = "both"), "side must be one of")
})

# run_exogeneity_tests ------------------------------------------------------

test_that("run_exogeneity_tests assembles IV and p-uniform results", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  df <- make_exogeneity_df()
  res <- run_exogeneity_tests(df, default_exogeneity_options())

  expect_true(all(c("iv", "puniform", "summary") %in% names(res)))
  expect_true(is.data.frame(res$summary))
  expect_equal(nrow(res$summary), 7L)
  expect_true("IV" %in% colnames(res$summary))
  expect_true("First-stage F" %in% res$summary$Metric)

  effect_est <- res$iv$coefficients$estimate[res$iv$coefficients$term == "effect"]
  expect_equal(effect_est, 0.5, tolerance = 0.05)
})

test_that("run_exogeneity_tests flags a weak instrument in the summary table", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  df <- make_weak_instrument_df()
  res <- run_exogeneity_tests(df, default_exogeneity_options(iv_instrument = "1/sqrt(n_obs)"))

  fstat_row <- res$summary[res$summary$Metric == "First-stage F", "IV"]
  expect_true(grepl("weak instrument", fstat_row))
})

test_that("run_exogeneity_tests skips gracefully when columns are missing", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  res <- run_exogeneity_tests(data.frame(effect = 1:3, se = rep(1, 3)), default_exogeneity_options())

  expect_true(!is.null(res$skipped))
  expect_true(grepl("study_id", res$skipped))
})

test_that("run_exogeneity_tests aborts when a required package is absent", {
  local_options(artma.verbose = 1)
  df <- make_exogeneity_df(n = 30)

  local_pretend_packages_absent("AER")
  expect_error(run_exogeneity_tests(df, default_exogeneity_options()), regexp = "AER")
})

test_that("build_exogeneity_summary survives a zero-length test statistic", {
  # A NULL AR statistic (test not computable) used to shorten the IV column
  # and abort the table assembly with "replacement has N rows, data has 7".
  iv_results <- list(
    coefficients = data.frame(
      term = c("effect", "publication_bias"),
      estimate_formatted = c("0.100", "0.200"),
      std_error_formatted = c("(0.010)", "(0.020)"),
      n_obs = c(120L, 120L),
      stringsAsFactors = FALSE
    ),
    instrument_name = "1/sqrt(n_obs)",
    ar_fstat = NULL,
    first_stage_fstat = NULL,
    weak_instrument = FALSE
  )
  puniform_results <- list(coefficients = NULL)
  options <- list(round_to = 3L)

  summary <- build_exogeneity_summary(iv_results, puniform_results, options)

  expect_equal(nrow(summary), 7L)
  expect_equal(summary[["IV"]][[7]], "not computable")
  expect_true(all(nzchar(summary[["p-Uniform*"]])))
})

test_that("build_exogeneity_summary does not print a bare NA for a non-estimable effect", {
  puniform_results <- list(
    coefficients = data.frame(
      term = c("effect", "publication_bias_test"),
      estimate = c(NA_real_, NA_real_),
      statistic = c(NA_real_, NA_real_),
      p_value = c(NA_real_, NA_real_),
      estimate_formatted = c(NA_character_, NA_character_),
      std_error_formatted = c(NA_character_, NA_character_),
      n_obs = c(60L, 60L),
      stringsAsFactors = FALSE
    )
  )

  summary <- build_exogeneity_summary(list(coefficients = NULL), puniform_results, list(round_to = 3L))

  expect_false(any(summary[["p-Uniform*"]] == "NA"))
  expect_equal(summary[["p-Uniform*"]][[3]], "not computable")
})
